package sjson
package json

/**
 * @author <a href="http://debasishg.blogspot.com">Debasish Ghosh</a>
 */
object Serializer {
  trait SJSON extends JsBean {
  
    import dispatch.json._
    import dispatch.json.Js._
    import java.io.{ObjectInputStream, ObjectOutputStream, ByteArrayInputStream, ByteArrayOutputStream}
    import org.apache.commons.io.input.ClassLoaderObjectInputStream

    val classLoader: Option[ClassLoader]

    import scala.reflect.Manifest
    def deepClone[T](obj: T)(implicit m: Manifest[T]): T = in[T](out(obj.asInstanceOf[AnyRef]))
  
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
  
    def in[T: Manifest](bytes: Array[Byte]): T = {
      in[T](new String(bytes, "UTF-8")) // (m)
    }

    def in[T: Manifest](json: String): T = {
      in[T](Js(json)) // (m)
    }

    private[json] def in[T: Manifest](js: JsValue): T = {
      val m = implicitly[Manifest[T]]
      // Map and Tuple2 both are serialized as Maps wrapped within a JsObject
      if (m.erasure == classOf[collection.immutable.Map[_, _]] ||
          m.erasure == classOf[Tuple2[_, _]]) extract[T](js)

      // beans are also serialized as JsObjects, but need to invoke fromJSON for beans
      else if (js.isInstanceOf[JsObject]) fromJSON(js, Some(m.erasure)).asInstanceOf[T]

      // all other cases
      else extract[T](js)
    }

    private[json] def extract[T: Manifest](jsv: JsValue): T = {
      val m = implicitly[Manifest[T]]
      val ex = jsv match {
        case JsNumber(n) => n
        case JsString(s) => s
        case JsArray(l) => {
          // deep serialization
          val inm = m.typeArguments.head
          l.map(in(_)(inm))
        }
        case JsBoolean(b) => b
        case JsNull => null
        case JsObject(mp) => { // either Map or a Tuple2
          // deep serialization
          val targs = m.typeArguments
          val (km, vm) = (targs.head, targs.last)

          if (m.erasure == classOf[Tuple2[_, _]]) {
            val tup = mp.toList.head
            val deserl_1 = in(tup._1)(km)
            val deserl_2 = in(tup._2)(vm)
            (deserl_1, deserl_2)
          } else { // Map
            mp.map { case (k, v) =>
              val deserl_k = in(k)(km)
              val deserl_v = in(v)(vm)
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
