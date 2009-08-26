package sjson.json

/**
 * @author <a href="http://debasishg.blogspot.com">Debasish Ghosh</a>
 */
object Serializer {
  object SJSON {
  
    import dispatch.json._
    import dispatch.json.Js._
    import Implicits._
  
    def deepClone(obj: AnyRef): AnyRef = in(out(obj), None)
  
    def out(obj: AnyRef): Array[Byte] = {
      try {
        JsValue.toJson(JsValue.apply(obj)).getBytes("UTF-8")
      } catch {
        case e: scala.MatchError =>
          JsBean.toJSON(obj).getBytes("UTF-8")
      }
    }
  
    def in[T](bytes: Array[Byte], clazz: Option[Class[T]]): AnyRef = clazz match {
      case None =>
        Js(new String(bytes, "UTF-8"))
      case _ =>
        JsBean.fromJSON(Js(new String(bytes, "UTF-8")), clazz).asInstanceOf[AnyRef]
      }
    
    def in(json: String): AnyRef = Js(json).asInstanceOf[AnyRef]
  }
}
  
