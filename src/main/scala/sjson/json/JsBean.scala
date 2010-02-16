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
            val inner = 
              if (cl.isAssignableFrom(classOf[Option[_]])) {
                val an = context.get.getDeclaredField(e._1.self).getAnnotation(classOf[OptionTypeHint])
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
              val ann = context.get.getDeclaredField(e._1.self).getAnnotation(classOf[JSONTypeHint])
              (Some(context.get.getDeclaredField(e._1.self)), 
                    Map() ++ 
                      (ann match {
                        case null => {
                          e._2.self.asInstanceOf[Map[_, _]]
                                   .map(y => (y._1.asInstanceOf[JsValue].self, y._2.asInstanceOf[JsValue].self))
                          }
                        case _ =>
                          e._2.self.asInstanceOf[Map[_, _]]
                                   .map(y => (y._1.asInstanceOf[JsValue].self, fromJSON(y._2.asInstanceOf[JsValue], Some(ann.value))))
                      }))
                    
            } else {
              // data member is an object which comes as Map in JSON
              (Some(context.get.getDeclaredField(e._1.self)), fromJSON(e._2.asInstanceOf[JsValue], Some(inner)))
            }
          }
          
          case x: List[_] => {
            val ann = context.get.getDeclaredField(e._1.self).getAnnotation(classOf[JSONTypeHint])
            ann match {
              case null => 
                (Some(context.get.getDeclaredField(e._1.self)), 
                  x.asInstanceOf[List[_]].map(y => y.asInstanceOf[JsValue].self))

              case _ =>
                (Some(context.get.getDeclaredField(e._1.self)), 
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
  def toJSON[T <: AnyRef](obj: T)(implicit ignoreProps: List[String]): String = {
    if (obj == null) quote("null")
    else {
    
      val clazz = obj.niceClass
    
      // handle primitives
      if (clazz.isPrimitive || 
        classOf[Number].isAssignableFrom(clazz) || 
        clazz.equals(classOf[Boolean]) ||
        clazz.equals(classOf[java.lang.Boolean])) obj.toString
    
      // handle string
      else if (obj.isInstanceOf[String]) quote(obj.asInstanceOf[String])

      // handle date
      else if (obj.isInstanceOf[java.util.Date]) quote(obj.asInstanceOf[java.util.Date].getTime.toString)
      
      // handle sequences & maps
      else if (obj.isInstanceOf[Seq[_ <: AnyRef]]) {
        obj.asInstanceOf[Seq[_ <: AnyRef]]
           .map(e => toJSON(e))
           .mkString("[", ",", "]")
      }
      else if (obj.isInstanceOf[Map[_ <: AnyRef, _ <: AnyRef]]) {
        obj.asInstanceOf[Map[_ <: AnyRef, _ <: AnyRef]]
           .map(e => toJSON(e._1.toString) + ":" + toJSON(e._2))
           .mkString("{", ",", "}")
      }
      else if (obj.isInstanceOf[Tuple2[_ <: AnyRef, _ <: AnyRef]]) {
        val (e1, e2) = obj.asInstanceOf[Tuple2[_ <: AnyRef, _ <: AnyRef]]
        "{" + toJSON(e1.toString) + ":" + toJSON(e2) + "}"
      }
    
      // handle beans
      else {
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
            val isOption = rv.isInstanceOf[Option[_]]
            
            // Use the value if the option is defined, otherwise ignore
            val rval =
              if (isOption) {
                val o = rv.asInstanceOf[Option[_]]
                if (o.isDefined) o.get.asInstanceOf[AnyRef] else null
              }
              else rv
            
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
    // println("clazz = " + clazz)
    // clazz.getDeclaredConstructors.foreach(println)
    // println("after")
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
