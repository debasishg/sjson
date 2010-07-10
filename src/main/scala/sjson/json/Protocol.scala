package sjson
package json

import dispatch.json._

trait Writes[T] {
  def writes(o: T): JsValue
}

trait Reads[T] {
  def reads(json: JsValue): T
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

trait DefaultProtocol extends Protocol {
  implicit object IntFormat extends Format[Int] {
    def writes(o: Int) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.intValue
      case _ => throw new RuntimeException("Int expected")
    }
  }

  implicit object ShortFormat extends Format[Short] {
    def writes(o: Short) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.shortValue
      case _ => throw new RuntimeException("Short expected")
    }
  }

  implicit object LongFormat extends Format[Long] {
    def writes(o: Long) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.longValue
      case _ => throw new RuntimeException("Long expected")
    }
  }

  implicit object FloatFormat extends Format[Float] {
    def writes(o: Float) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.floatValue
      case _ => throw new RuntimeException("Float expected")
    }
  }

  implicit object DoubleFormat extends Format[Double] {
    def writes(o: Double) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.doubleValue
      case _ => throw new RuntimeException("Double expected")
    }
  }

  implicit object BooleanFormat extends Format[Boolean] {
    def writes(o: Boolean) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsTrue => true
      case JsFalse => false
      case _ => throw new RuntimeException("Boolean expected")
    }
  }

  implicit object StringFormat extends Format[String] {
    def writes(o: String) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsString(s) => s
      case _ => throw new RuntimeException("String expected")
    }
  }
}

import JsonSerialization._
object CollectionProtocol extends DefaultProtocol {
  abstract class ListFormat[T](implicit jT: Format[T]) extends Format[List[T]] {
    def writes(ts: List[T]) = JsArray(ts.map(t => tojson(t)(jT)))
    def reads(json: JsValue) = json match {
      case JsArray(ts) => ts.map(t => fromjson(t)(jT))
      case _ => throw new RuntimeException("List expected")
    }
  }
  implicit object ListIntFormat extends ListFormat[Int]
  implicit object ListShortFormat extends ListFormat[Short]
  implicit object ListLongFormat extends ListFormat[Long]
  implicit object ListDoubleFormat extends ListFormat[Double]
  implicit object ListFloatFormat extends ListFormat[Float]
  implicit object ListStringFormat extends ListFormat[String]
  implicit object ListBooleanFormat extends ListFormat[Boolean]

  import scala.reflect.Manifest
  abstract class ArrayFormat[T](implicit jT: Format[T], mf: Manifest[T]) extends Format[Array[T]] {
    def writes(ts: Array[T]) = JsArray((ts.map(t => tojson(t)(jT))).toList)
    def reads(json: JsValue) = json match {
      case JsArray(ts) => listToArray(ts.map(t => fromjson(t)(jT)))
      case _ => throw new RuntimeException("Array expected")
    }
  }

  def listToArray[T: Manifest](ls: List[T]): Array[T] = ls.toArray

  implicit object ArrayIntFormat extends ArrayFormat[Int]
  implicit object ArrayShortFormat extends ArrayFormat[Short]
  implicit object ArrayLongFormat extends ArrayFormat[Long]
  implicit object ArrayDoubleFormat extends ArrayFormat[Double]
  implicit object ArrayFloatFormat extends ArrayFormat[Float]
  implicit object ArrayStringFormat extends ArrayFormat[String]
  implicit object ArrayBooleanFormat extends ArrayFormat[Boolean]

  abstract class MapFormat[K, V](implicit jK: Format[K], jV: Format[V]) extends Format[Map[K, V]] {
    def writes(ts: Map[K, V]) = JsObject(ts.map{case (k, v) => ((tojson(k.toString)).asInstanceOf[JsString], tojson(v)(jV))})
    def reads(json: JsValue) = json match {
      case JsObject(m) => Map() ++ m.map{case (k, v) => (fromjson(k)(jK), fromjson(v)(jV))}
      case _ => throw new RuntimeException("Map expected")
    }
  }
}

object JsonSerialization {
  def tojson[T](o: T)(implicit tjs: Writes[T]): JsValue = {
    tjs.writes(o)
  }

  def fromjson[T](json: JsValue)(implicit fjs: Reads[T]): T = {
    fjs.reads(json)
  }
}
