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

  private def makeOptionFromHint[T: Manifest](obj: T, f: Field) = f.getAnnotation(classOf[OptionTypeHint]) match {
    case null => obj
    case x if (x.value.isAssignableFrom(implicitly[Manifest[T]].erasure)) => Some(obj)
    case x => obj
  }

  /**
   * Process the <tt>JsValue</tt> and return the wrapped value. It may need to take care of two things:
   * <li>in case of <tt>JsArray</tt>, need to map over and do a deep processing</li>
   * <li>in case the field is annotated with an <tt>Option</tt> type that matches the wrapped type, then <tt>Some(obj)</tt></li>
   */
  private def processJsValue(js: JsValue, f: Option[Field] = None): Any = js match {
    case JsArray(l) =>  f match {
      case Some(fld) => makeOptionFromHint(l.map(j => processJsValue(j)), fld)
      case _ => l.map(j => processJsValue(j))
    }
    case JsString(s) => f match {
      case Some(fld) => makeOptionFromHint(s, fld)
      case _ => s
    }
    case JsNumber(n) => f match {
      case Some(fld) => makeOptionFromHint(n, fld)
      case _ => n
    }
    case JsTrue => f match {
      case Some(fld) => makeOptionFromHint(true, fld)
      case _ => true
    }
    case JsFalse => f match {
      case Some(fld) => makeOptionFromHint(false, fld)
      case _ => false
    }
    case JsNull => null
    case JsObject(m) => f match {
      case Some(fld) => {
        val ann = fld.getAnnotation(classOf[OptionTypeHint]) 
        ann match {
          case null => 
            val jsType = fld.getAnnotation(classOf[JSONTypeHint])

            if (jsType != null) {
              m.map {case (y1: JsValue, y2: JsValue) => 
                (y1.self, fromJSON(y2, Some(jsType.value)))}
            } else { // just a Map
              m.map {case (y1: JsValue, y2: JsValue) => 
                (y1.self, processJsValue(y2))}
            }

          case x => 
            val jsType = fld.getAnnotation(classOf[JSONTypeHint])

            if (jsType != null) {
              m.map {case (y1: JsValue, y2: JsValue) => 
                (y1.self, Some(processJsValue(y2)))}
            } else Some(fromJSON(js, Some(x.value)))
        }
      }
      // no field: treat as a Map
      case None => 
        m.map {case (y1: JsValue, y2: JsValue) => 
          (y1.self, processJsValue(y2))
        }
    }
  }

  /**
   * Process Map recursively and consider the impact of a <tt>JSONTypeHint</tt> annotation.
   */
  private def processMap(m: Map[_,_], field: Field) = {
    val ann = field.getAnnotation(classOf[JSONTypeHint])
    (Some(field), 
      Map() ++ 
        (ann match {
          case null =>
            m.map {case (y1: JsValue, y2: JsValue) => 
              (y1.self, processJsValue(y2, Some(field)))
            }
          case x if x.value.isPrimitive == true =>
            // remember all numbers are converted to BigDecimal by the JSON parser
            m.map {case (y1: JsValue, y2: JsValue) =>
              if (y2.isInstanceOf[JsNumber]) (y1.self, mkNum(y2.self.asInstanceOf[BigDecimal], ann.value))
              else (y1.self, y2.self)
            }
          case _ =>
            m.map {case (y1: JsValue, y2: JsValue) => y2 match {
              case JsArray(l) => (y1.self, l.map(j => fromJSON(j, Some(ann.value), field)))
              case _ => (y1.self, fromJSON(y2, Some(ann.value), field))
            }}
         }))
  }

  /**
   * Process Tuple2 recursively and consider the impact of a <tt>JSONTypeHint</tt> annotation.
   */
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

  private def processSingletonObject[T](fqn: String): Either[Exception, T] = {
    getClassFor(fqn) match {
      case Left(ex) => Left(new Exception("Cannot get class info for :" + fqn))
      case Right(clazz) => getObjectFor(clazz) 
    }
  }
  
  /**
   * Convert the value to an Enumeration.Value instance using class <tt>enumObjectClass</tt>'s 
   * valueOf method. Returns an instance of <tt>Enumeration.Value</tt>.
   */
  private def toEnumValue[T](value: Any, enumObjectClass: Class[T]): Enumeration#Value = {
    if (Modifier.isAbstract(enumObjectClass.getModifiers)) {
      throw new IllegalArgumentException("cannot get type information for enum " + value)
    }
    val method = enumObjectClass.getMethod("withName", classOf[String])
    method.invoke(null, value.asInstanceOf[String]).asInstanceOf[Enumeration#Value]
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
   * Convert the <tt>JsValue</tt> to an instance of the class <tt>context</tt>, using the parent for 
   * any annotation hints. Returns an instance of <tt>T</tt>.
   */
  private[json] def fromJSON[T](js: JsValue, context: Option[Class[T]], parent: Field): T = {
    if (context.isDefined && classOf[Enumeration#Value].isAssignableFrom(context.get)) 
      toEnumValue(js.self, 
        getEnumObjectClass(context.get.asInstanceOf[Class[Enumeration#Value]], parent)).asInstanceOf[T]
    else fromJSON(js, context)
  }

  /**
   * Convert the <tt>JsValue</tt> to an instance of the class <tt>context</tt>. Returns an instance of
   * <tt>T</tt>.
   */
  def fromJSON[T](js: JsValue, context: Option[Class[T]]): T = {
    if (!js.isInstanceOf[JsObject] || !context.isDefined) {
      js match {
        case JsArray(l) => processJsValue(js).asInstanceOf[T]
        case JsString(s) if (s endsWith "$") => 
          processSingletonObject(s) match {
            case Left(ex) => sys.error("Cannot make object for :" + s)
            case Right(obj) => obj.asInstanceOf[T]
          }
        case _ => js.self.asInstanceOf[T]
      }
    }
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
                  x.map{case y: JsValue => 

                    // list of Maps : can be objects or can be scala.collection.Map
                    if (y.isInstanceOf[JsObject]) {

                      // use field to find out if it's a Map or an object
                      if (field.getGenericType.isInstanceOf[java.lang.reflect.ParameterizedType]) {
                        field.getGenericType
                             .asInstanceOf[java.lang.reflect.ParameterizedType]
                             .getActualTypeArguments.head match {

                          // case Map
                          case h: java.lang.reflect.ParameterizedType if (h.getRawType.equals(classOf[scala.collection.immutable.Map[_, _]])) => processJsValue(y, Some(field))

                          // case object
                          case h => fromJSON(y, Some(ann.value), field)
                        }
                      } else fromJSON(y, Some(ann.value), field)
                    } else fromJSON(y, Some(ann.value), field)
                  })
             }
          }
        
          // ending with $ means either a singleton object or it can be a string
          // containing $ as the last character. Hence the additional check on the type
          case x: String if ((x endsWith "$") && context.get.getDeclaredField(props.get(name).get).getType.equals(classOf[java.lang.String]) == false) =>
            processSingletonObject(x) match {
              case Left(ex) => sys.error("Cannot make object for :" + x)
              case Right(obj) => (Some(context.get.getDeclaredField(props.get(name).get)), obj)
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
                else if (z.isInstanceOf[String] && (z == null)) null
                else z

              // need to handle Option[] in individual fields
              if (y.getType.isAssignableFrom(classOf[scala.Option[_]])) {
                // handle None case which comes as an empty List since we serialize None as []
                if (num.isInstanceOf[List[_]] && num.asInstanceOf[List[_]].isEmpty) y.set(instance, None)
                else if (num.isInstanceOf[BigDecimal]) {
                  val inner = getInnerTypeForOption(y.getType, y)
                  y.set(instance, Some(mkNum(num, inner)))
                }
                else y.set(instance, Some(num))
              } else y.set(instance, num)
            }
          }
        }
      }
    }
  }

  private def getObject[T](fqn: String, clazz: Class[T]) = getObjectFor[T](fqn)

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
  def toJSON[T <: AnyRef](obj: T): String = obj match {
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

    case Some(s) => toJSON(s.asInstanceOf[AnyRef])

    case None => "[]"

    case (s: Array[AnyRef]) => 
      s.map(e => toJSON(e)).mkString("[", ",", "]")

    case (s: Array[_]) => 
      s.mkString("[", ",", "]")

    case (m: Map[AnyRef, AnyRef]) =>
      m.map(e => toJSON(e._1.toString) + ":" + toJSON(e._2))
       .mkString("{", ",", "}")

    case (t: Tuple2[AnyRef, AnyRef]) =>
        "{" + toJSON(t._1) + ":" + toJSON(t._2) + "}"

    // scala case object
    case x if x.getClass.getName.endsWith("$") => {
      quote(obj.getClass.getName)
    }

    case _ => {
      // handle beans
      val clazz = obj.niceClass

      // just an observation:
      // if the class is not at the top most level, then annotating the class
      // with @BeanInfo does not work. Need to annotate every property with @BeanProperty
      val pds = 
        Introspector.getBeanInfo(clazz)
          .getPropertyDescriptors
          .filter(_.getName != "class")


      if (pds.isEmpty) {
        throw new UnsupportedOperationException("Class " + clazz + " not supported for conversion")
      }
        
      val props =
        for {
          pd <- pds
          val rm = pd.getReadMethod
          val rv = rm.invoke(obj)

          val ann = rm.getAnnotation(classOf[JSONProperty])
          val v =
            if (ann == null || ann.value == null || ann.value.length == 0) pd.getName
            else ann.value

          val isNull = rv match {
            case null => true
            case o: Option[_] => o.isEmpty
            case x: Seq[_] => x.isEmpty
            case a: Array[_] => a.isEmpty 
            case _ => false
          }

          if (ann == null || !ann.ignore)
          if (ann == null || !(ann.ignoreIfNull && isNull))

        } yield toJSON(v) + ":" + toJSON(rv)

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
      clazz.getDeclaredConstructors
           .filter(_.getParameterTypes.length == 0)
           .headOption.getOrElse(sys.error("no default constructor found on " + clazz))

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
