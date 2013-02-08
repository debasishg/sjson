package sjson
package json

import scala.reflect.runtime.universe._
import Util._

/**
 * @author <a href="http://debasishg.blogspot.com">Debasish Ghosh</a>
 */
object Serializer {
  trait SJSON extends JsBean {
  
    import dispatch.classic.json._
    import dispatch.classic.json.Js._
    import java.io.{ObjectInputStream, ObjectOutputStream, ByteArrayInputStream, ByteArrayOutputStream}
    import org.apache.commons.io.input.ClassLoaderObjectInputStream

    val classLoader: Option[ClassLoader]

    def deepClone[T: TypeTag](obj: T): T = in[T](out(obj.asInstanceOf[AnyRef]))
  
    /**
     * Serialize out a Scala object. It can be serialized back in to the object using
     * <tt>in</tt> method.
     * <p/>
     * <pre>
     * val l = List("ab", "cd")
     * in(out(l)) => ["ab", "cd"]
     * in[List[String]](out(l)) => List("ab", "cd")
     * </pre>
     * <em>Caveat</em>
     * Nulls are serialized as String null ("null"). This may create problems if a String field
     * contains the value "null".
     */
    def out(obj: AnyRef): Array[Byte] = {
      try {
        JsValue.toJson(JsValue.apply(obj)).getBytes("UTF-8")
      } catch {
        case e: scala.MatchError => toJSON(obj).getBytes("UTF-8")
      }
    }
  
    def in[T: TypeTag](bytes: Array[Byte]): T = {
      in[T](new String(bytes, "UTF-8")) // (m)
    }

    def in[T: TypeTag](json: String): T = {
      in[T](Js(json)) // (m)
    }

    def in[T: TypeTag](js: JsValue): T = {
      in_impl[T](js, typeOf[T])
    }

    private[json] def in_impl[T: TypeTag](js: JsValue, tpe: Type): T = {
      // Map and Tuple2 both are serialized as Maps wrapped within a JsObject
      if (tpe <:< typeOf[collection.immutable.Map[_, _]] ||
          tpe <:< typeOf[Tuple2[_, _]]) extractFromJs[T](js, tpe)

      // beans are also serialized as JsObjects, but need to invoke fromJSON for beans
      else if (js.isInstanceOf[JsObject]) 
        fromJSON(js, Some(getClassFromScalaType(tpe).asInstanceOf[Class[T]]))

      // all other cases
      else extractFromJs[T](js, tpe)
    }


    private[json] def extractFromJs[T: TypeTag](jsv: JsValue, tpe: Type): T = {
      val ex = jsv match {
        case JsNumber(n) => n
        case JsString(s) => s
        case JsArray(l) => {
          // deep serialization
          val intpe = tpe.typeSymbol.asType.typeParams.head.asType.toTypeIn(tpe)
          l.map(in_impl(_, intpe))
        }
        case JsBoolean(b) => b
        case JsNull => null
        case JsObject(mp) => { // either Map or a Tuple2
          // deep serialization
          val targs = tpe.typeSymbol.asType.typeParams.map(_.asType.toTypeIn(tpe))
          val (km, vm) = (targs.head, targs.last)

          if (tpe <:< typeOf[Tuple2[_, _]]) {
            val tup = mp.toList.head
            val deserl_1 = in_impl(tup._1, km)
            val deserl_2 = in_impl(tup._2, vm)

            (deserl_1, deserl_2)
          } else { // Map
            mp.map { case (k, v) =>
              val deserl_k = in_impl(k, km)
              val deserl_v = in_impl(v, vm)
              (deserl_k, deserl_v)
            }
          }
        }
      }
      ex.asInstanceOf[T]
    }

    /**
     * Serialize in a JSON into a Scala object, specifying a class that can be loaded
     * through an externally specified class loader. 
     * In order to specify the class loader, do the following :
     * <pre>
     * object SJSON extends SJSON {
     *   val classLoader = None
     * }
     * </pre>
     */
    def in(json: Array[Byte], clazzName: String): AnyRef = {
      val clazz =
        classLoader match {
          case Some(cl) =>
            Class.forName(clazzName, true, cl)
          case None =>
            Class.forName(clazzName)
        }
      fromJSON(Js(new String(json)), Some(clazz)).asInstanceOf[AnyRef]
    }
  }

  object SJSON extends SJSON with DefaultConstructor {
    val classLoader = Some(this.getClass.getClassLoader)
  }
}
