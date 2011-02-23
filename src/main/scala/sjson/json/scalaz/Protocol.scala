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

trait DefaultProtocol extends StandardTypes with Generic with Primitives {

  import dispatch.json._
  import Js._
  import JsonSerialization._

  // field[String](('lastName ! str), js)
  // field[Address](('address ! obj), js)
  def field[T](name: String, js: JsValue)(implicit fjs: Reads[T]): Validation[NonEmptyList[String], T] = {
    val JsObject(m) = js
    m.get(JsString(name)).map(fromjson[T](_)(fjs)).getOrElse(("field " + name + " not found").fail.liftFailNel)
  }

  // field[String]((('address ! obj) andThen ('city ! str)), js)
  def field[T](f: (JsValue => JsValue), js: JsValue)(implicit fjs: Reads[T]): Validation[NonEmptyList[String], T] = try {
    fromjson[T](f(js))(fjs)
  } catch {
    case e: Exception => e.getMessage.fail.liftFailNel
  }

  // curried version
  def field_c[T](name: String)(implicit fjs: Reads[T]) = { js: JsValue =>
    val JsObject(m) = js
    m.get(JsString(name)).map(fromjson[T](_)(fjs)).getOrElse(("field " + name + " not found").fail.liftFailNel)
  }

  // curried version
  def field_c[T](f: (JsValue => JsValue))(implicit fjs: Reads[T]) = { js: JsValue =>
    try {
      fromjson[T](f(js))(fjs)
    } catch {
      case e: Exception => e.getMessage.fail.liftFailNel
    }
  }
}

object DefaultProtocol extends DefaultProtocol
