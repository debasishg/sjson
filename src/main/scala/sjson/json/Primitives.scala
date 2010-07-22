package sjson
package json

import dispatch.json._

trait Primitives extends Protocol {
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
