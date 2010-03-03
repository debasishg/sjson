package sjson.json

import dispatch.json._

trait JsBean {
  
  implicit def string2Class[T<:AnyRef](name: String)(implicit classLoader: ClassLoader): Class[T] = {
    val clazz = Class.forName(name, true, classLoader)
    clazz.asInstanceOf[Class[T]]
  }
  
  private [json] def lookupType[T](parent: Class[T], name: String): Class[_] = {
    parent.getDeclaredField(name).getType
  }
  
  class NiceObject[T <: AnyRef](x : T) {
    def niceClass : Class[_ <: T] = x.getClass.asInstanceOf[Class[T]]
  }
  implicit def toNiceObject[T <: AnyRef](x : T) = new NiceObject(x)
  
  import java.beans._
  
  import java.lang.reflect._
  import dispatch.json._
  import dispatch.json.Js._
  import Util._
  
  /**
   * Convert the <tt>JsValue</tt> to an instance of the class <tt>context</tt>. Returns an instance of
   * <tt>T</tt>.
   */
  def fromJSON[T](js: JsValue, context: Option[Class[T]]): T = { 
    if (!js.isInstanceOf[JsObject] || !context.isDefined) js.self.asInstanceOf[T]
    else {
      val m = js.self.asInstanceOf[Map[JsString, JsValue]]
      val fields = context.get.getDeclaredFields
    
      // property names for the bean
      val props = fields map(_.getName)

      /**
       * for some bean properties, json property may have different names by virtue of
       * being annotated with <tt>JSONProperty</tt>. Keep a map of the two names for
       * later substitution.
       */
      val annotatedProps =
        (Map[String, String]() /: fields)((b, a) => 
          if (a.getAnnotation(classOf[JSONProperty]) != null)
            b + (a.getAnnotation(classOf[JSONProperty]).value -> a.getName)
          else b)

      val info = m.map {e =>
        e._2.self match {
        
          /**
           * ignore additional properties in JSON that don't match bean property and
           * do not have <tt>JSONProperty</tt> annotation. If a JSON property name is
           * not found directly in the bean, but exists as the target value of a 
           * <tt>JSONProperty</tt> annotation in the bean, then it needs to be considered.
           */
          case x if (props.exists(y => y.equals(e._1.self)) == false) => 
            annotatedProps get(e._1.self) match {
              case Some(y) => (Some(context.get.getDeclaredField(y)), 
                               fromJSON(e._2.asInstanceOf[JsValue], Some(context.get.getDeclaredField(y).getType)))
              case _ => (None, null)
            }
        
          /**
           * Can be a Map in either of the following cases:
           * 1. the data members is really a scala.Collection.Map 
           * 2. the data member can be an object which comes in JSON as a Map
           */
          case x: Map[_, _] => {
            val cl = lookupType(context.get, e._1.self)
            val field = context.get.getDeclaredField(e._1.self)
            val inner = 
              if (cl.isAssignableFrom(classOf[Option[_]])) {
                val an = field.getAnnotation(classOf[OptionTypeHint])
                an match {
                  case null =>
                    throw new IllegalArgumentException("cannot get type information")
                  case _ =>
                    an.value
                }
              } else { cl }

            // data member is a Map
            if (inner.isAssignableFrom(classOf[Map[_, _]])) {
              // the value of the Map may have an annotated type
              val ann = field.getAnnotation(classOf[JSONTypeHint])
              (Some(field), 
                    Map() ++ 
                      (ann match {
                        case null =>
                          x.map {case (y1: JsValue, y2: JsValue) => (y1.self, y2.self)}
                        case _ =>
                          x.asInstanceOf[Map[_, _]]
                          x.map {case (y1: JsValue, y2: JsValue) => (y1.self, fromJSON(y2, Some(ann.value)))}
                      }))
                    
            } else {
              if (inner.isAssignableFrom(classOf[Tuple2[_, _]])) {
                // fixme: ignoring annotations and generic types for the time being
                val (t1: JsValue, t2: JsValue) = x.toList.first
                (Some(field), (t1.self, t2.self))
              }
              else
                // data member is an object which comes as Map in JSON
                (Some(field), fromJSON(e._2.asInstanceOf[JsValue], Some(inner)))
            }
          }
          
          case x: List[_] => {
            val field = context.get.getDeclaredField(e._1.self)
            val ann = field.getAnnotation(classOf[JSONTypeHint])
            ann match {
              case null => 
                (Some(field), 
                  x.asInstanceOf[List[_]].map(y => y.asInstanceOf[JsValue].self))

              case _ =>
                (Some(field), 
                  x.asInstanceOf[List[_]].map(y => fromJSON(y.asInstanceOf[JsValue], Some(ann.value))))
            }
          }
        
          case x => 
            (Some(context.get.getDeclaredField(e._1.self)), e._2.self)
        }
      }

      newInstance(context.get) { instance =>
        info.foreach {x => 
          x match {
            case (None, _) =>
            case (Some(y), z) => {
              y.setAccessible(true)

              // type conversion hacks
              val num = 
                // json parser makes BigDecimal out of all numbers
                if (z.isInstanceOf[BigDecimal]) mkNum(z.asInstanceOf[BigDecimal], y.getType) 

                // if it's date, need to make one from JSON string
                else if (y.getType.isAssignableFrom(classOf[java.util.Date])) mkDate(z.asInstanceOf[String])

                // as ugly as it gets
                // arrays in Scala are boxed sometimes: need to unbox before set
                // kludge to take care of the fact that both array and List have the same JSON representation
                // here the field is an Array but the value is a List
                else if (y.getType.isArray) {
                  z.asInstanceOf[List[_]]
                   .toArray.asInstanceOf[scala.runtime.BoxedAnyArray]
                   .unbox(y.getType.getComponentType)
                }
                  
                // special treatment for JSON "nulls"
                else if (z.isInstanceOf[String] && (z == "null")) null
                else z

              // need to handle Option[] in individual fields
              if (y.getType.isAssignableFrom(classOf[scala.Option[_]]))
                y.set(instance, Some(num)) else y.set(instance, num) 
            }
          }
        }
      }
    }
  }

  /**
   * Generate a JSON representation of the object <tt>obj</tt> and return the string.
   */
  def toJSON[T <: AnyRef](obj: T)(implicit ignoreProps: List[String]): String = obj match {
    case null => quote("null")
    case (n: Number) => obj.toString
    case (b: java.lang.Boolean) => obj.toString
    case (s: String) => quote(obj.asInstanceOf[String])
    case (d: java.util.Date) => 
      quote(obj.asInstanceOf[java.util.Date].getTime.toString)

    case (s: Seq[AnyRef]) =>
      s.map(e => toJSON(e)).mkString("[", ",", "]")

    case (m: Map[AnyRef, AnyRef]) =>
      m.map(e => toJSON(e._1) + ":" + toJSON(e._2))
       .mkString("{", ",", "}")

    case (t: Tuple2[AnyRef, AnyRef]) =>
        "{" + toJSON(t._1) + ":" + toJSON(t._2) + "}"

    case _ => {
      // handle beans
      val clazz = obj.niceClass

      // just an observation:
      // if the class is not at the top most level, then annotating the class
      // with @BeanInfo does not work. Need to annotate every property with @BeanProperty
      val pds = 
        Introspector.getBeanInfo(clazz)
          .getPropertyDescriptors
          .filter(e => ignoreProps.exists(_.equals(e.getName)) == false)

      if (pds.isEmpty) {
        throw new UnsupportedOperationException("Class " + clazz + " not supported for conversion")
      }
        
      val props = 
        for {
          pd <- pds
          val rm = pd.getReadMethod
          val rv = rm.invoke(obj, null)
            
          // Option[] needs to be treated differently
          val (rval, isOption) = rv match {
            case (o: Option[_]) =>
              if (o.isDefined) (o.get.asInstanceOf[AnyRef], true) else (null, true)
            case x => (x, false)
          }
            
          val ann = rm.getAnnotation(classOf[JSONProperty])
          val v = 
            if (ann == null || ann.value == null || ann.value.length == 0) pd.getName 
            else ann.value
          val ignore = 
            if (ann != null) ann.ignore || (rv == null && ann.ignoreIfNull) else false

          if ((ignore == false) && (!isOption || (isOption && rval != null)))
        } yield toJSON(v) + ":" + toJSON(rval)
      
      props.mkString("{", ",", "}")
    }
  }

  def newInstance[T](clazz: Class[T])(op: T => Unit): T
}

/**
 * Use this trait with JsBean to instantiate classes using a default private constructor. This is the default.
 */
trait DefaultConstructor {
  import java.lang.reflect._

  def newInstance[T](clazz: Class[T])(op: T => Unit): T = {
    // need to access private default constructor .. hack!
    // clazz.getDeclaredConstructors.foreach(println)
    val constructor =
      clazz.getDeclaredConstructors.filter(_.getParameterTypes.length == 0).first

     if (!Modifier.isPublic(constructor.getModifiers()) ||
      !Modifier.isPublic(constructor.getDeclaringClass().getModifiers()))
        constructor.setAccessible(true)

    val v = constructor.newInstance().asInstanceOf[T]
    op(v)
    v
  }
}

object JsBean extends JsBean with DefaultConstructor

/**
 * Use this trait with JsBean to instantiate classes using Objenesis. 
 *
 * This is faster and negates the need for a default no-args constructor. 
 * However it adds a runtime dependency on objenesis.jar.
 *
 * @see http://objenesis.googlecode.com/svn/docs/index.html
 * @author Joe Walnes
 */
trait Objenesis {
  import org.objenesis.ObjenesisStd
  
  val objenesis = new ObjenesisStd
  
  def newInstance[T](clazz: Class[T])(op: T => Unit): T = {
    val v = objenesis.newInstance(clazz).asInstanceOf[T]
    op(v)
    v
  }
}
