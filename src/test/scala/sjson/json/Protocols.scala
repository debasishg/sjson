package sjson
package json

import DefaultProtocol._

object Protocols {
  case class Person(lastName: String, firstName: String, age: Int)
  object PersonProtocol extends DefaultProtocol {
    import dispatch.json._
    import JsonSerialization._
    implicit object PersonFormat extends Format[Person] {
      def reads(json: JsValue): Person = json match {
        case JsObject(m) =>
          Person(fromjson[String](m(JsString("lastName"))), 
            fromjson[String](m(JsString("firstName"))), fromjson[Int](m(JsString("age"))))
        case _ => throw new RuntimeException("JsObject expected")
      }
      def writes(p: Person): JsValue =
        JsObject(List(
          (tojson("lastName").asInstanceOf[JsString], tojson(p.lastName)), 
          (tojson("firstName").asInstanceOf[JsString], tojson(p.firstName)), 
          (tojson("age").asInstanceOf[JsString], tojson(p.age)) ))
    }
  }

  case class Shop(store: String, item: String, price: Int)
  implicit val ShopFormat: Format[Shop] = 
    asProduct3("store", "item", "price")(Shop)(Shop.unapply(_).get)

  case class Address(street: String, city: String, zip: String)
  implicit val AddressFormat: Format[Address] = 
    asProduct3("street", "city", "zip")(Address)(Address.unapply(_).get)

  case class Contact(name: String, addresses: List[Address])
  implicit val ContactFormat: Format[Contact] = 
    asProduct2("name", "addresses")(Contact)(Contact.unapply(_).get)

  case class Account(no: String, name: String, addresses: Array[Address])
  implicit val AccountFormat: Format[Account] = 
    asProduct3("no", "name", "addresses")(Account)(Account.unapply(_).get)

  case class Base(no: String, name: String, addresses: Array[Address])
  implicit val BaseFormat: Format[Base] = 
    asProduct3("no", "name", "addresses")(Base)(Base.unapply(_).get)

  class Derived(no: String, name: String, addresses: Array[Address], special: Boolean) 
    extends Base(no, name, addresses) {
    val specialFlag = special
  }
  object DerivedProtocol extends DefaultProtocol {
    import dispatch.json._
    import JsonSerialization._
    implicit object DerivedFormat extends Format[Derived] {
      def reads(json: JsValue): Derived = {
        val b = fromjson[Base](json)
        json match {
          case JsObject(m) =>
            new Derived(b.no, b.name, b.addresses,
              fromjson[Boolean](m(JsString("specialFlag"))))
          case _ => throw new RuntimeException("JsObject expected")
        }
      }
      def writes(a: Derived): JsValue = {
        val o = tojson(a: Base)
        val JsObject(m) = o
        JsObject(m ++ List((tojson("specialFlag").asInstanceOf[JsString], tojson(a.specialFlag))))
      }
    }
  }

  case class Name(name: String)
  implicit val NameFormat: Format[Name] = wrap[Name, String]("name")(_.name, Name)

  case class Holder(item: List[String])
  implicit val HolderFormat: Format[Holder] = wrap[Holder, List[String]]("item")(_.item, Holder)

  import TestBeans._
  implicit val AddressWithOptionalCityFormat: Format[AddressWithOptionalCity] =
    asProduct3("street", "city", "zip")(AddressWithOptionalCity)(AddressWithOptionalCity.unapply(_).get)

  import dispatch.json._
  trait HttpType
  implicit val HttpTypeFormat: Format[HttpType] = new Format[HttpType] {
    def reads(json: JsValue): HttpType = json match {
      case JsString("Get") => Get
      case JsString("Post") => Post
      case _ => error("Invalid HttpType")
    }
    def writes(a: HttpType): JsValue = a match {
      case Get => JsString("Get")
      case Post => JsString("Post")
    }
  }

  case object Get extends HttpType
  case object Post extends HttpType

  case class Http(url: String, t: HttpType)
  implicit val HttpFormat: Format[Http] = 
    asProduct2("url", "t")(Http)(Http.unapply(_).get)

  case class Bar(name: String, list: Option[List[Foo]])
  case class Foo(name: String, list: List[Bar])
  implicit val BarFormat: Format[Bar] = lazyFormat(asProduct2("name", "list")(Bar)(Bar.unapply(_).get))
  implicit val FooFormat: Format[Foo] = lazyFormat(asProduct2("name", "list")(Foo)(Foo.unapply(_).get))

  case class JobStart(name: String, start: WeekDay.Value)
  object JobStartProtocol extends DefaultProtocol {
    import JsonSerialization._
    implicit object JobStartFormat extends Format[JobStart] {
      def reads(json: JsValue): JobStart = json match {
        case JsObject(m) =>
          JobStart(fromjson[String](m(JsString("name"))), 
            WeekDay.withName(fromjson[String](m(JsString("start")))))
        case _ => throw new RuntimeException("JsObject expected")
      }
      def writes(p: JobStart): JsValue =
        JsObject(List(
          (tojson("name").asInstanceOf[JsString], tojson(p.name)), 
          (tojson("start").asInstanceOf[JsString], tojson(p.start.toString)))) 
    }
  }
}
