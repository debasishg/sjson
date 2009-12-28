package sjson.json

/**
 * @author <a href="http://debasishg.blogspot.com">Debasish Ghosh</a>
 */
object Serializer {
  trait SJSON {
  
    import dispatch.json._
    import dispatch.json.Js._
    import Implicits._
    import java.io.{ObjectInputStream, ObjectOutputStream, ByteArrayInputStream, ByteArrayOutputStream}
    import org.apache.commons.io.input.ClassLoaderObjectInputStream

    val classLoader: Option[ClassLoader]

    import scala.reflect.Manifest
    def deepClone[T](obj: T)(implicit m: Manifest[T]): AnyRef = in[T](out(obj.asInstanceOf[AnyRef]))
  
    /**
     * Serialize out a Scala object into JSON.
     * <p/>
     * <em>Caveat</em>
     * Nulls are serialized as String null ("null"). This may create problems if a String field
     * contains the value "null".
     */
    def out(obj: AnyRef): Array[Byte] = {
      val bos = new ByteArrayOutputStream
      val out = new ObjectOutputStream(bos)

      try {
        out.writeUTF(JsValue.toJson(JsValue.apply(obj)))
        out.close
        bos.toByteArray
      } catch {
        case e: scala.MatchError =>
          out.writeUTF(JsBean.toJSON(obj))
          out.close
          bos.toByteArray
      }
    }
  
    def in[T](bytes: Array[Byte])(implicit m: Manifest[T]): AnyRef = {
      val ins = 
        if (classLoader.isDefined) 
          new ClassLoaderObjectInputStream(
            classLoader.get, new ByteArrayInputStream(bytes))
        else new ObjectInputStream(new ByteArrayInputStream(bytes))

      in[T](ins.readUTF)(m)
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
      case "java.lang.Object" =>
        Js(json)
      case "scala.runtime.Nothing$" =>
        Js(json)
      case "None.type" =>
        Js(json)
      case _ =>
        JsBean.fromJSON(Js(json), Some(m.erasure)).asInstanceOf[AnyRef]
    }

    // def in(json: String): AnyRef = Js(json) 
  }

  object SJSON extends SJSON {
    val classLoader = None
  }
}
  
