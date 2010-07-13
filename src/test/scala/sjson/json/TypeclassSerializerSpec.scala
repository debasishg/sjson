package sjson
package json

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TypeclassSerializerSpec extends Spec with ShouldMatchers {

  import JsonSerialization._

  describe("Serialization of simple objects") {
    it("should serialize into json and back") {
      import Protocols._
      import ShopProtocol._
      val shop = Shop("Shoppers Stop", "dress material", 1000)
      fromjson[Shop](tojson(shop)) should equal(shop)
    }
  }

  describe("Serialization of lists") {
    it ("should serialize list of Ints") {
      import CollectionProtocol.ListIntFormat
      val l1 = List(100, 200, 300, 400)
      fromjson(tojson(l1)) should equal(l1)
    }

    it ("should serialize list of Strings") {
      import CollectionProtocol.ListStringFormat
      val l2 = List("dg", "mc", "rc", "nd")
      fromjson(tojson(l2)) should equal(l2)
    }
  }

  describe("Serialization of Maps") {
    it ("should serialize Map of Strings & Strings") {
      object MapProtocol extends DefaultProtocol {
        implicit object MapStringStringFormat extends CollectionProtocol.MapFormat[String, String]
      }
      import MapProtocol.MapStringStringFormat
      val m = Map("100" -> "dg", "200" -> "mc")
      fromjson(tojson(m)) should equal(m)
    }
  }

  describe("Serialization of composite objects") {
    it("should serialize into json and back") {
      import Protocols._
      import ContactProtocol._

      val contact = Contact("Debasish Ghosh", 
        List(Address("monroe st", "denver", "80231"), Address("pine drive", "santa clara", "95054")))
      fromjson[Contact](tojson(contact)) should equal(contact)
    }
  }

  describe("Serialization of composite objects with arrays") {
    it("should serialize into json and back") {
      import Protocols._
      import AccountProtocol._

      val account = Account("123", "Debasish Ghosh", 
        Array(Address("monroe st", "denver", "80231"), Address("pine drive", "santa clara", "95054")))

      val ac = fromjson[Account](tojson(account))
      ac.no should equal(account.no)
      ac.name should equal(account.name)
      ac.addresses should be === account.addresses
    }
  }
}
