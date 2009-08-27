package sjson.json

/**
 * @author <a href="http://debasishg.blogspot.com">Debasish Ghosh</a>
 */
object Serializer {
  object SJSON {
  
    import dispatch.json._
    import dispatch.json.Js._
    import Implicits._
  
    import scala.reflect.Manifest
    def deepClone[T](obj: T)(implicit m: Manifest[T]): AnyRef = in[T](out(obj.asInstanceOf[AnyRef]))
  
    def out(obj: AnyRef): Array[Byte] = {
      try {
        JsValue.toJson(JsValue.apply(obj)).getBytes("UTF-8")
      } catch {
        case e: scala.MatchError =>
          JsBean.toJSON(obj).getBytes("UTF-8")
      }
    }
  
    def in[T](bytes: Array[Byte])(implicit m: Manifest[T]): AnyRef = {
      in[T](new String(bytes, "UTF-8"))(m)
    }
    
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
}
  
