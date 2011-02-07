package sjson
package json.scalaz

import scalaz._
import Scalaz._

import dispatch.json._

trait Writes[T] {
  def writes(o: T): JsValue
}

trait Reads[T] {
  def reads(json: JsValue): Validation[NonEmptyList[String], T]
}

trait Format[T] extends Writes[T] with Reads[T]

trait Protocol {
  implicit val IntFormat: Format[Int]
  implicit val ShortFormat: Format[Short]
  implicit val LongFormat: Format[Long]
  implicit val BooleanFormat: Format[Boolean]
  implicit val FloatFormat: Format[Float]
  implicit val DoubleFormat: Format[Double]
  implicit val StringFormat: Format[String]
}

// trait DefaultProtocol extends CollectionTypes with Generic with Primitives
trait DefaultProtocol extends Primitives with StandardTypes with CollectionTypes
object DefaultProtocol extends DefaultProtocol {
  import JsonSerialization._
  def field[T](name: String, js: JsValue)(implicit fjs: Reads[T]): Validation[NonEmptyList[String], T] = {
    val JsObject(m) = js
    m.get(JsString(name)).map(fromjson[T](_)(fjs)).getOrElse(("field " + name + " not found").fail.liftFailNel)
  }
}
