package sjson
package json

import dispatch.json._
import Js._
import JsonSerialization._

object Protocols {
  case class Shop(store: String, item: String, price: Int)
  object ShopProtocol extends DefaultProtocol {
    implicit object ShopFormat extends Format[Shop] {
      def writes(o: Shop) = {
        JsObject(
          List(
            (tojson("store").asInstanceOf[JsString], tojson(o.store)), 
            (tojson("item").asInstanceOf[JsString], tojson(o.item)), 
            (tojson("price").asInstanceOf[JsString], tojson(o.price))
          ))
      }
      def reads(js: JsValue) = js match {
        case JsObject(m) => // m is the Map
          Shop(
            fromjson[String](m(JsString("store"))), 
            fromjson[String](m(JsString("item"))), 
            fromjson[Int](m(JsString("price")))
          )
        case _ => throw new RuntimeException("object expected")
      }
    }
  }

  case class Address(street: String, city: String, zip: String)
  object AddressProtocol extends DefaultProtocol {
    implicit object AddressFormat extends Format[Address] {
      def writes(o: Address) = {
        JsObject(
          List(
            (tojson("street").asInstanceOf[JsString], tojson(o.street)), 
            (tojson("city").asInstanceOf[JsString], tojson(o.city)), 
            (tojson("zip").asInstanceOf[JsString], tojson(o.zip))
          ))
      }
      def reads(js: JsValue) = js match {
        case JsObject(m) => // m is the Map
          Address(
            fromjson[String](m(JsString("street"))), 
            fromjson[String](m(JsString("city"))), 
            fromjson[String](m(JsString("zip")))
          )
        case _ => throw new RuntimeException("object expected")
      }
    }
  }

  case class Contact(name: String, addresses: List[Address])
  import AddressProtocol._
  import CollectionProtocol.ListFormat
  implicit object ListAddressFormat extends ListFormat[Address]
  object ContactProtocol extends DefaultProtocol {
    implicit object ContactFormat extends Format[Contact] {
      def writes(o: Contact) = {
        JsObject(
          List(
            (tojson("name").asInstanceOf[JsString], tojson(o.name)), 
            (tojson("addresses").asInstanceOf[JsString], tojson(o.addresses)) 
          ))
      }
      def reads(js: JsValue) = js match {
        case JsObject(m) => // m is the Map
          Contact(
            fromjson[String](m(JsString("name"))), 
            fromjson[List[Address]](m(JsString("addresses")))
          )
        case _ => throw new RuntimeException("object expected")
      }
    }
  }

  case class Account(no: String, name: String, addresses: Array[Address])
  import AddressProtocol._
  import CollectionProtocol.ArrayFormat
  implicit object ArrayAddressFormat extends ArrayFormat[Address]
  object AccountProtocol extends DefaultProtocol {
    implicit object AccountFormat extends Format[Account] {
      def writes(o: Account) = {
        JsObject(
          List(
            (tojson("no").asInstanceOf[JsString], tojson(o.no)), 
            (tojson("name").asInstanceOf[JsString], tojson(o.name)), 
            (tojson("addresses").asInstanceOf[JsString], tojson(o.addresses)) 
          ))
      }
      def reads(js: JsValue) = js match {
        case JsObject(m) => // m is the Map
          Account(
            fromjson[String](m(JsString("no"))), 
            fromjson[String](m(JsString("name"))), 
            fromjson[Array[Address]](m(JsString("addresses")))
          )
        case _ => throw new RuntimeException("object expected")
      }
    }
  }
}
