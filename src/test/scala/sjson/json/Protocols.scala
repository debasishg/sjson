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
          (tojson[String]("lastName").asInstanceOf[JsString], tojson[String](p.lastName)), 
          (tojson[String]("firstName").asInstanceOf[JsString], tojson[String](p.firstName)), 
          (tojson[String]("age").asInstanceOf[JsString], tojson[Int](p.age)) ))
    }
  }

  case class Shop(store: String, item: String, price: Int)
  object ShopProtocol extends DefaultProtocol {
    implicit val ShopFormat: Format[Shop] = 
      asProduct3("store", "item", "price")(Shop)(Shop.unapply(_).get)
  }

  case class Address(street: String, city: String, zip: String)
  object AddressProtocol extends DefaultProtocol {
    implicit val AddressFormat: Format[Address] = 
      asProduct3("street", "city", "zip")(Address)(Address.unapply(_).get)
  }

  case class Contact(name: String, addresses: List[Address])
  import AddressProtocol._
  object ContactProtocol extends DefaultProtocol {
    implicit val ContactFormat: Format[Contact] = 
      asProduct2("name", "addresses")(Contact)(Contact.unapply(_).get)
  }

  case class Account(no: String, name: String, addresses: Array[Address])
  import AddressProtocol._
  object AccountProtocol extends DefaultProtocol {
    implicit val AccountFormat: Format[Account] = 
      asProduct3("no", "name", "addresses")(Account)(Account.unapply(_).get)
  }
}
