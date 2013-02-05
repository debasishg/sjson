package sjson
package json

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

  import java.util.Date
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
   */
  def instantiate(typ: Type, params: Map[Name, Any]) = {
    println(params)
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
    cm.reflectConstructor(ctor)(ps.map(e => params.get(e._1).get): _*)
  }
}

