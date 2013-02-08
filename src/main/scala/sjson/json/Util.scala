package sjson
package json

import java.util.{Date, TimeZone}
object Util {
  def quote(s: String): String = s match {
    case null => "null"
    case _ => {
      new StringBuilder(s.length + 2)
        .append('"')
        .append(s.foldLeft(new StringBuilder(""))((a, b) => a.append(escape(b, '"'))).toString)
        .append('"')
        .toString
    }
  }
  
  private def escape(c: Char, quoteChar: Char): String = c match {
    case '"' if (c == quoteChar) => "\\" + c
    case '"' => "" + c
    case '\'' if (c == quoteChar) => "\\" + c
    case '\'' => "" + c
    case '/' => "\\/"
    case '\\' => "\\\\"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case x if (x < 32 || x > 126) => {
      val hex = Integer.toHexString(x)
      val len = hex.length
      "\\u" + (if (len < 4) "0000".substring(len) + hex else hex)
    }
    case _ => "" + c
  }
  
  import java.io._
  def readTillNl(in: InputStreamReader): String = {
    var c = -1
    var str = new StringBuffer
    do {
      c = in.read
      if (c != '\n' && c != -1) {
        str.append(c.toChar)
      }
    } 
    while (c != '\n' && c != -1)
    str.toString
  }

  def mkNum(v: Any, c: Class[_]) = {
    v match {
      case b: BigDecimal =>
        if (c.isAssignableFrom(classOf[Int])) b.intValue
        else if (c.isAssignableFrom(classOf[Long])) b.longValue
        else if (c.isAssignableFrom(classOf[Float])) b.floatValue
        else if (c.isAssignableFrom(classOf[Double])) b.doubleValue
        else if (c.isAssignableFrom(classOf[Short])) b.shortValue
        else b
      case _ => sys.error("unexpected")
    }
  }

  def mkDate(v: String): Date = {
    new Date(v.toLong.longValue)
  }

  val loader = getClass.getClassLoader

  def getObjectFor[T](fqn: String, classloader: ClassLoader = loader): Either[Exception, T] = try {
    getClassFor(fqn, classloader) match {
      case Right(value) ⇒
        val instance = value.getDeclaredField("MODULE$")
        instance.setAccessible(true)
        val obj = instance.get(null)
        if (obj eq null) Left(new NullPointerException) else Right(obj.asInstanceOf[T])
      case Left(exception) ⇒ Left(exception) //We could just cast this to Either[Exception, T] but it's ugly
    }
  } catch {
    case e: Exception ⇒
      Left(e)
  }

  def getObjectFor[T](clazz: Class[T]): Either[Exception, T] = {
    val instance = clazz.getDeclaredField("MODULE$")
    instance.setAccessible(true)
    val obj = instance.get(null)
    if (obj eq null) Left(new NullPointerException) else Right(obj.asInstanceOf[T])
  }

  def getClassFor[T](fqn: String, classloader: ClassLoader = loader): Either[Exception, Class[T]] = try {
    assert(fqn ne null)

    // First, use the specified CL
    val first = try {
      Right(classloader.loadClass(fqn).asInstanceOf[Class[T]])
    } catch {
      case c: ClassNotFoundException ⇒ Left(c)
    }

    if (first.isRight) first
    else {
      // Second option is to use the ContextClassLoader
      val second = try {
        Right(Thread.currentThread.getContextClassLoader.loadClass(fqn).asInstanceOf[Class[T]])
      } catch {
        case c: ClassNotFoundException ⇒ Left(c)
      }

      if (second.isRight) second
      else {
        val third = try {
          if (classloader ne loader) Right(loader.loadClass(fqn).asInstanceOf[Class[T]]) else Left(null) //Horrid
        } catch {
          case c: ClassNotFoundException ⇒ Left(c)
        }

        if (third.isRight) third
        else {
          try {
            Right(Class.forName(fqn).asInstanceOf[Class[T]]) // Last option is Class.forName
          } catch {
            case c: ClassNotFoundException ⇒ Left(c)
          }
        }
      }
    }
  } catch {
    case e: Exception ⇒ Left(e)
  }

  import scala.reflect.runtime.universe._
  def getParamTypes[A](x: A)(implicit ev: TypeTag[A]) = {
    ev.tpe match {
      case TypeRef(_, _, args) => args
    }
  }

  // get Java class from Scala type (scala.reflect.runtime.universe.Type)
  // use mirrors
  // http://stackoverflow.com/questions/12901823/any-way-to-obtain-a-java-class-from-a-scala-2-10-type-tag-or-symbol
  def getClassFromScalaType(t: Type) = {
    val m = runtimeMirror(getClass.getClassLoader)
    m.runtimeClass(t.typeSymbol.asClass)
  }

  def substringFromLastSep(s: String, sep: String) = s.substring(s.lastIndexOf(sep) + 1, s.length)

  /** Invoke method <tt>method</tt> on object <tt>obj</tt> of type <tt>typ</tt>.
   *  You will have a nasty surprise if <tt>method</tt> is not a valid method of
   *  <tt>obj</tt>.
   */
  def getPropertyValue(obj: Any, typ: Type, property: String) = {
    val t = typ.declaration(newTermName(property)).asTerm.accessed.asTerm
    val m = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(obj)
    im.reflectField(t).get
  }

  def setPropertyValue(obj: Any, typ: Type, property: String, value: Any) = {
    val t = typ.declaration(newTermName(property)).asTerm.accessed.asTerm
    val m = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(obj)
    im.reflectField(t).set(value)
  }

  /** Instantiate a class of type <tt>typ</tt> using the various mirrors
   *  available. 
   *
   *  Need to invoke the proper constructor that matches the parameters specified
   *  by <tt>params</tt>. The current implementation just matches by length of
   *  param list for the constructor.
   */
  def instantiate(typ: Type, params: Map[Name, Any]) = {

    /** Constructor parameters may have to be pre-processed before
     *  passing to the constructor. e.g. Options are serialized with
     *  the actual value, if present. But during de-serialization, we
     *  need to pass the <tt>Some</tt> if the data is present.
     */
    def prepareConstructorParams(ps: List[(Name, Type)]) = {
      ps.map {p =>
        val v = params.get(p._1).get
        // if (p._2 <:< typeOf[Option[_]]) {
          // v match {
            // case List() => None // remember we serialize None as empty list []
            // case _ => Some(v)
          // }
        // } else if (p._2 =:= typeOf[java.util.Date]) {
        if (p._2 =:= typeOf[java.util.Date]) {
          mkDate(v.asInstanceOf[String])
        } else if (p._2 =:= typeOf[java.util.TimeZone]) {
          TimeZone.getTimeZone(v.asInstanceOf[String])
        } else if (v.isInstanceOf[BigDecimal]) {
          makeNumber(v.asInstanceOf[BigDecimal], p._2)
        } else v
      }
    }

    def makeNumber(n: BigDecimal, tpe: Type) = {
      if (tpe =:= typeOf[Int]) n.intValue
      else if (tpe =:= typeOf[Long]) n.longValue
      else if (tpe =:= typeOf[Float]) n.floatValue
      else if (tpe =:= typeOf[Double]) n.doubleValue
      else if (tpe =:= typeOf[Short]) n.shortValue
      else n
    }

    // get runtime mirror (JavaMirror)
    val m = runtimeMirror(getClass.getClassLoader)

    // get class
    val cls = typ.typeSymbol.asClass

    // get class mirror
    val cm = m.reflectClass(cls)

    // get all constructors
    val alts = typ.declaration(nme.CONSTRUCTOR).asTerm.alternatives

    // get the no of params that the constructor need to have &
    // match the appropriate constructor
    val pcount = params.size

    // get the constructor and the params that match in count with the 
    // size of params list passed as argument
    val cps = alts.collect {case ms: MethodSymbol if ms.paramss.head.size == pcount => (ms, ms.paramss)}

    val ctor = cps.head._1
    val prms = cps.head._2

    // get the param details & the type signature
    val ps = prms.head.map(e => (e.name, e.typeSignature))
    val paramsToCtor = prepareConstructorParams(ps)

    cm.reflectConstructor(ctor)(paramsToCtor: _*)
  }

  def getEnumType(str: String, tpe: Type) = {
    val ru = runtimeMirror(getClass.getClassLoader)
    val obj = tpe.termSymbol.asModule
    val mm = ru.reflectModule(obj)
    val instance = mm.instance
    instance.asInstanceOf[Enumeration].withName(str)
  }
}

/**
scala> val m = typeOf[WeekDay.Value].members.filter(!_.isMethod).head.typeSignat
ure.members.filter(a => a.isMethod && a.name == newTermName("withName"))
m: Iterable[reflect.runtime.universe.Symbol] = SynchronizedOps(method withName)

scala> val ru = runtimeMirror(getClass.getClassLoader)
ru: reflect.runtime.universe.Mirror = JavaMirror with scala.tools.nsc.interprete
r.IMain$TranslatingClassLoader@5472184f of type class scala.tools.nsc.interprete
r.IMain$TranslatingClassLoader with classpath [(memory)] and parent being scala.
tools.nsc.util.ScalaClassLoader$URLClassLoader@e9a15d9 of type class scala.tools
.nsc.util.ScalaClassLoader$URLClassLoader with classpath [file:/D:/software/java
7/jre/lib/resources.jar,file:/D:/software/java7/jre/lib/rt.jar,file:/D:/software
/java7/jre/lib/jsse.jar,file:/D:/software/java7/jre/lib/jce.jar,file:/D:/softwar
e/java7/jre/lib/charsets.jar,file:/D:/software/java7/jre/lib/ext/dnsns.jar,file:
/D:/software/java7/jre/lib/ext/localedata.jar,file:/D:/software/java7/jre/lib/ex
t/sunec.jar,file:/D:/software/java7/jre/lib/ext/sunjce_provider.jar,file:/D:/...

scala> val im = ru.reflect(WeekDay)
im: reflect.runtime.universe.InstanceMirror = instance mirror for WeekDay

scala> m.head match {
     |   case ms: MethodSymbol => im.reflectMethod(ms)
     |   case _ => sys.error("error")
     | }
res45: reflect.runtime.universe.MethodMirror = method mirror for scala.Enumerati
on.withName(s: String): Enumeration.this.Value (bound to WeekDay)

scala> res45("Monday")
res50: Any = Monday
**/
