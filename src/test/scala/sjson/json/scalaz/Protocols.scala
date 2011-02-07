package sjson
package json.scalaz

import scalaz._
import Scalaz._

import DefaultProtocol._
import JsonSerialization._
import dispatch.json._

object Protocols {
  case class Address(no: Int, street: String, city: String, zip: String)
  case class Person(lastName: String, firstName: String, age: Int, address: Address)

  object AddressProtocol extends DefaultProtocol {

    implicit object AddressFormat extends Format[Address] {
      def reads(json: JsValue): ValidationNEL[String, Address] = json match {
        case m@JsObject(_) => 
          (field[Int]("no", m) |@| field[String]("street", m) |@| field[String]("city", m) |@| field[String]("zip", m)) { Address }

        case _ => "JsObject expected".fail.liftFailNel
      }

      def writes(p: Address): JsValue =
        JsObject(List(
          (tojson("no").asInstanceOf[JsString], tojson(p.no)), 
          (tojson("street").asInstanceOf[JsString], tojson(p.street)), 
          (tojson("city").asInstanceOf[JsString], tojson(p.city)),
          (tojson("zip").asInstanceOf[JsString], tojson(p.zip)) ))
    }
  }

  object PersonProtocol extends DefaultProtocol {
    import AddressProtocol._

    implicit object PersonFormat extends Format[Person] {
      def reads(json: JsValue): ValidationNEL[String, Person] = json match {
        case m@JsObject(_) => 
          (field[String]("lastName", m) |@| field[String]("firstName", m) |@| field[Int]("age", m) |@| field[Address]("address", m)) { Person }

        case _ => "JsObject expected".fail.liftFailNel
      }
      def writes(p: Person): JsValue =
        JsObject(List(
          (tojson("lastName").asInstanceOf[JsString], tojson(p.lastName)), 
          (tojson("firstName").asInstanceOf[JsString], tojson(p.firstName)), 
          (tojson("age").asInstanceOf[JsString], tojson(p.age)),
          (tojson("address").asInstanceOf[JsString], tojson(p.address)) ))
    }
  }

  object IncorrectPersonProtocol extends DefaultProtocol {
    import AddressProtocol._

    implicit object PersonFormat extends Format[Person] {
      def reads(json: JsValue): ValidationNEL[String, Person] = json match {
        case m@JsObject(_) => 
          (field[String]("LastName", m) |@| field[String]("firstname", m) |@| field[Int]("age", m) |@| field[Address]("address", m)) { Person }

        case _ => "JsObject expected".fail.liftFailNel
      }
      def writes(p: Person): JsValue =
        JsObject(List(
          (tojson("lastName").asInstanceOf[JsString], tojson(p.lastName)), 
          (tojson("firstName").asInstanceOf[JsString], tojson(p.firstName)), 
          (tojson("age").asInstanceOf[JsString], tojson(p.age)),
          (tojson("address").asInstanceOf[JsString], tojson(p.address)) ))
    }
  }
}
