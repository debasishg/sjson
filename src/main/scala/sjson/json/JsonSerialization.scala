package sjson
package json

import dispatch.json._

object JsonSerialization {
  def tojson[T](o: T)(implicit tjs: Writes[T]): JsValue = {
    tjs.writes(o)
  }

  def fromjson[T](json: JsValue)(implicit fjs: Reads[T]): T = {
    fjs.reads(json)
  }
}
