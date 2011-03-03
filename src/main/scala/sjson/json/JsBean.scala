package sjson
package json

import java.lang.reflect.Modifier
import java.util.TimeZone

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
  
  import java.lang.reflect.Field
  import dispatch.json._
  import Util._

  private def getProps[T](clazz: Class[T]) = {
    val fields = clazz.getMethods
    Map() ++
    fields.map {field =>
      val a = field.getAnnotation(classOf[JSONProperty])
      a match {
        case null => (field.getName, field.getName)
        case x if x.value.length > 0 => 
          (x.value, field.getName)
        case x => (field.getName, field.getName)
      }
    }
  }

  private def getInnerTypeForOption[T](clazz: Class[T], field: Field) = {
    if (clazz.isAssignableFrom(classOf[Option[_]])) {
      // get the inner type from the OptionTypeHint annotation
      val an = field.getAnnotation(classOf[OptionTypeHint])
      an match {
        case null =>
          throw new IllegalArgumentException("cannot get type information")
        case _ =>
          an.value
      }
    } else { clazz }
  }

  private def processMap(m: Map[_,_], field: Field) = {
    val ann = field.getAnnotation(classOf[JSONTypeHint])
    (Some(field), 
      Map() ++ 
        (ann match {
          case null =>
            m.map {case (y1: JsValue, y2: JsValue) => 
              (y1.self, y2.self)
            }
          case x if x.value.isPrimitive == true =>
            // remember all numbers are converted to BigDecimal by the JSON parser
            m.map {case (y1: JsValue, y2: JsValue) =>
              if (y2.isInstanceOf[JsNumber]) (y1.self, mkNum(y2.self.asInstanceOf[BigDecimal], ann.value))
              else (y1.self, y2.self)
            }
          case _ =>
            m.map {case (y1: JsValue, y2: JsValue) => 
              (y1.self, fromJSON(y2, Some(ann.value), field))
            }
         }))
  }

  private def processTuple2(t: Tuple2[_,_], field: Field) = {
    val (t1: JsValue, t2: JsValue) = t
    val ann = field.getAnnotation(classOf[JSONTypeHint])
    (Some(field), 
      (ann match {
        case null =>
          (t1.self, t2.self)
        case x if x.value.isPrimitive == true =>
          // remember all numbers are converted to BigDecimal by the JSON parser
          if (t2.isInstanceOf[JsNumber]) (t1.self, mkNum(t2.self.asInstanceOf[BigDecimal], ann.value))
          else (t1.self, t2.self)
        case _ =>
          (t1.self, fromJSON(t2, Some(ann.value), field))
       }))
  }
  
  /**
   * Convert the value to an Enumeration.Value instance using class <tt>enumObjectClass</tt>'s valueOf method. Returns an instance of
   * <tt>Enumeration.Value</tt>.
   */
  private def toEnumValue[T](value: Any, enumObjectClass: Class[T]): Enumeration#Value = {
    if (Modifier.isAbstract(enumObjectClass.getModifiers)) {
      throw new IllegalArgumentException("cannot get type information for enum " + value)
    }
    val method = enumObjectClass.getMethod("valueOf", classOf[String])
    method.invoke(null, value.asInstanceOf[String]).asInstanceOf[Option[Enumeration#Value]].get
  }

  private def getEnumObjectClass[T <: Enumeration#Value](targetClass: Class[T], y: Field): Class[_] = {
    val enumObjectClass = y.getAnnotation(classOf[EnumTypeHint]) match {
      case null =>
        targetClass.getEnclosingClass
      case an => Class.forName(an.value)
    }
    enumObjectClass
  }

  /**
   * Convert the <tt>JsValue</tt> to an instance of the class <tt>context</tt>, using the parent for any annotation hints.
   * Returns an instance of <tt>T</tt>.
   */
  private[json] def fromJSON[T](js: JsValue, context: Option[Class[T]], parent: Field): T = {
    if (context.isDefined && classOf[Enumeration#Value].isAssignableFrom(context.get)) {
      toEnumValue(js.self, getEnumObjectClass(context.get.asInstanceOf[Class[Enumeration#Value]], parent)).asInstanceOf[T]
    } else {
      fromJSON(js, context)
    }
  }

  /**
   * Convert the <tt>JsValue</tt> to an instance of the class <tt>context</tt>. Returns an instance of
   * <tt>T</tt>.
   */
  def fromJSON[T](js: JsValue, context: Option[Class[T]]): T = {
    if (!js.isInstanceOf[JsObject] || !context.isDefined) js.self.asInstanceOf[T]
    else {
      // bean as a map from json
      val bean = js.self.asInstanceOf[Map[JsString, JsValue]]

      // properties of the bean class
      // as a map to take care of mappings for JSONProperty annotation
      val props = getProps(context.get)

      // iterate on name/value pairs of the bean
      val info = bean map {case (JsString(name), value) =>
        value.self match {
        
          // need to ignore properties in json that are not in props
          case x if (props.get(name).isDefined == false) =>
            (None, null)
        
          /**
           * Can be a Map in any of the following cases:
           * 1. the data member is really a scala.Collection.Map 
           * 2. tha data member is a Tuple2 which also we serialize as a Map
           * 3. the data member can be an object which comes in JSON as a Map
           */
          case x: Map[_, _] => {
            // type of the property from the bean class
            val cl = lookupType(context.get, props.get(name).get)

            // field
            val field = context.get.getDeclaredField(props.get(name).get)

            // can be an Option[_]
            val inner = getInnerTypeForOption(cl, field)

            inner match {
              // case 1
              case m if (m isAssignableFrom(classOf[Map[_,_]])) =>
                processMap(x, field)

              // case 2
              case t if (t isAssignableFrom(classOf[Tuple2[_,_]])) =>
                processTuple2(x.toList.head, field)

              // case 3
              case _ =>
                (Some(field), fromJSON(value, Some(inner), field))
            }
          }
          
          case x: List[_] => {
            val field = context.get.getDeclaredField(props.get(name).get)
            val ann = field.getAnnotation(classOf[JSONTypeHint])
            ann match {
              case null => 
                (Some(field), 
                  x.map{ case y: JsValue => y.self
                  })

              case a if a.value.isPrimitive == true => 
                (Some(field), 
                  x.map{case y: JsValue => 
                    // remember all numbers are converted to BigDecimal by the JSON parser
                    if (y.isInstanceOf[JsNumber]) mkNum(y.self.asInstanceOf[BigDecimal], ann.value)
                    else y.self
                  })

              case _ =>
                (Some(field), 
                  x.map{ case y: JsValue => fromJSON(y, Some(ann.value), field)
                  })
            }
          }
        
          case x => 
            (Some(context.get.getDeclaredField(props.get(name).get)), value.self)
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

                // if it's timezone, need to make one from JSON string
                else if (y.getType.isAssignableFrom(classOf[java.util.TimeZone])) TimeZone.getTimeZone(z.asInstanceOf[String])

                // if it's date, need to make one from JSON string
                else if (y.getType.isAssignableFrom(classOf[java.util.Date])) mkDate(z.asInstanceOf[String])

                // process Enumerations
                else if (classOf[Enumeration#Value].isAssignableFrom(y.getType)) {
                  toEnumValue(z, getEnumObjectClass(y.getType.asInstanceOf[Class[Enumeration#Value]], y))
                }

                // as ugly as it gets
                else if (y.getType.isArray) {
                  mkArray(z.asInstanceOf[List[_]], y.getType.getComponentType)
                }

                // special treatment for JSON "nulls"
                // else if (z.isInstanceOf[String] && (z == "null")) null
                else if (z.isInstanceOf[String] && (z == null)) null
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

  private def mkArray(l: List[_], clz: Class[_]): Array[_] = {
    import java.lang.reflect.{Array => JArray}
    val a = JArray.newInstance(clz, l.size)
    var i = 0
    while (i < l.size) {
      JArray.set(a, i, l(i))
      i += 1
    }
    a.asInstanceOf[Array[_]]
  }

  /**
   * Generate a JSON representation of the object <tt>obj</tt> and return the string.
   */
  def toJSON[T <: AnyRef](obj: T)(implicit ignoreProps: List[String]): String = obj match {
    // case null => quote("null")
    case null => "null"
    case (n: Number) => obj.toString
    case (b: java.lang.Boolean) => obj.toString
    case (s: String) => quote(obj.asInstanceOf[String])
    case (d: java.util.Date) => 
      quote(obj.asInstanceOf[java.util.Date].getTime.toString)

    case (d: java.util.TimeZone) => quote(d.getID)

    case (v: Enumeration#Value) => 
      quote(v toString)

    case (s: Seq[AnyRef]) =>
      s.map(e => toJSON(e)).mkString("[", ",", "]")

    case (s: Array[AnyRef]) =>
      s.map(e => toJSON(e)).mkString("[", ",", "]")

    case (m: Map[AnyRef, AnyRef]) =>
      m.map(e => toJSON(e._1.toString) + ":" + toJSON(e._2))
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
          val rv = rm.invoke(obj)

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
      clazz.getDeclaredConstructors.filter(_.getParameterTypes.length == 0).head

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
