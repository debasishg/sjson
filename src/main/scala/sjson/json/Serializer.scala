package sjson.json

/**
 * @author <a href="http://debasishg.blogspot.com">Debasish Ghosh</a>
 */
object Serializer {
  trait SJSON extends JsBean {
  
    import dispatch.json._
    import dispatch.json.Js._
    import Implicits._
    import java.io.{ObjectInputStream, ObjectOutputStream, ByteArrayInputStream, ByteArrayOutputStream}
    import org.apache.commons.io.input.ClassLoaderObjectInputStream

    val classLoader: Option[ClassLoader]

    import scala.reflect.Manifest
    def deepClone[T](obj: T)(implicit m: Manifest[T]): AnyRef = in[T](out(obj.asInstanceOf[AnyRef]))
  
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
  
    def in[T](bytes: Array[Byte])(implicit m: Manifest[T]): AnyRef = {
      in[T](new String(bytes, "UTF-8"))(m)
    }

    /**
     * Serialize in a JSON into a Scala object. 
     * <p/>
     * The API can be invoked either by specifying a concrete class, as <tt>in[Address](json)</tt>
     * then the API returns an instance of <tt>Address</tt>. If any of the fields of <tt>Address</tt>
     * object was <tt>null</tt>, then it will be appropriately converted to the <tt>null</tt> value.
     * However, if the API is invoked as <tt>in[AnyRef](json)</tt> or <tt>in(json)</tt> or
     * <tt>in[None](json)</tt>, then the API returns an instance of JsValue, which can be manipulated
     * using the Json extractors. e.g.
     * <pre>
     * val addr = Address("Market Street", "San Francisco", null)
     * val a = serializer.in[AnyRef](serializer.out(addr))
     *
     * val c = 'city ? str
     * val c(_city) = a
     * _city should equal("San Francisco")
     *
     * val s = 'street ? str
     * val s(_street) = a
     * _street should equal("Market Street")
     *
     * val z = 'zip ? str
     * val z(_zip) = a
     * _zip should equal("null") 
     * </pre>
     *
     * Note that in the second case, the zip field is being de-serialized as String null, "null",
     * which is how we serialize nulls in sjson.
     */
    def in[T](json: String)(implicit m: Manifest[T]): AnyRef = m.toString match {
      case "Object" =>
        Js(json)
      case "java.lang.Object" =>
        Js(json)
      case "scala.runtime.Nothing$" =>
        Js(json)
      case "Nothing" =>
        Js(json)
      case "None.type" =>
        Js(json)
      case _ =>
        fromJSON(Js(json), Some(m.erasure)).asInstanceOf[AnyRef]
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
