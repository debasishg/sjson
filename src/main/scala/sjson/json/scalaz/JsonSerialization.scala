package sjson
package json.scalaz

import scalaz._
import Scalaz._

import dispatch.json._

object JsonSerialization {
  def tojson[T](o: T)(implicit tjs: Writes[T]): JsValue = tjs.writes(o)

  def fromjson[T](json: JsValue)(implicit fjs: Reads[T]): Validation[String, T] = fjs.reads(json)

  def tobinary[T](o: T)(implicit tjs: Writes[T]): Array[Byte] = 
    JsValue.toJson(tojson(o)).getBytes("UTF-8")

  def frombinary[T](bytes: Array[Byte])(implicit fjs: Reads[T]): Validation[String, T] =
    fromjson(Js(new String(bytes, "UTF-8")))
}
