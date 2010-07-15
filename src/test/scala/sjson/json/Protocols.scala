package sjson
package json

import dispatch.json._
import Js._
import JsonSerialization._

object Protocols {
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
