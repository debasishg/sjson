package sjson
package json.scalaz

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TypeclassSerializerSpec extends Spec with ShouldMatchers {

  import DefaultProtocol._
  import JsonSerialization._
  import scalaz._
  import Scalaz._

  describe("Serialization using Person protocol") {
    it ("should serialize a Person") {
      import Protocols._
      import PersonProtocol._
      val a = Address(12, "Monroe Street", "Denver", "80231")
      val p = Person("ghosh", "debasish", 20, a)
      fromjson[Person](tojson(p)) should equal(p.success)
    }
  }

  describe("Serialization using incorrect Person protocol") {
    it ("serialization should fail") {
      import Protocols._
      import IncorrectPersonProtocol._
      val a = Address(12, "Monroe Street", "Denver", "80231")
      val p = Person("ghosh", "debasish", 20, a)
      (fromjson[Person](tojson(p))).fail.toOption.get.list should equal (List("field LastName not found", "field firstname not found"))
    }
  }

  describe("Serialization of lists") {
    it ("should serialize list of Ints") {
      val l1 = List(100, 200, 300, 400)
      fromjson[List[Int]](tojson(l1)) should equal(l1.success)
    }

    it ("should serialize list of Strings") {
      val l2 = List("dg", "mc", "rc", "nd")
      fromjson[List[String]](tojson(l2)) should equal(l2.success)
    }
  }
  val jsonString = 
    """{
         "lastName" : "ghosh", 
         "firstName" : "debasish", 
         "age" : 20, 
         "address" : { "no" : 12, "street" : "Monroe Street", "city" : "Denver", "zip" : "80231" }, 
         "phone" : { "no" : "3032144567", "ext" : 212 },
         "office" :
          {
            "name" : "anshinsoft",
            "address" : { "no" : 23, "street" : "Hampden Avenue", "city" : "Denver", "zip" : "80245" } 
          }
       }"""

  import Protocols._
  import AddressProtocol._
  import dispatch.json._
  import Js._

  val js = Js(jsonString)
  val c = Contact("ghosh","debasish",Address(12,"Monroe Street","Denver","80231"),"Denver",Address(23,"Hampden Avenue","Denver","80245"))

  describe("Serialization from arbitrary JSON string") {
    it ("should serialize into Contact") {

      (field[String]("lastName", js)    |@| 
        field[String]("firstName", js)   |@| 
        field[Address]("address", js)    |@| 
        field[String]("city", (('office ! obj) andThen ('address ? obj))(js)) |@|
        field[Address]((('office ! obj) andThen ('address ! obj)), js)) { Contact } should equal(c.success)
    }

    it ("should not serialize but report list of errors") {

      (field[String]("lastName", js)    |@| 
        field[String]("FirstName", js)   |@| 
        field[Address]("address", js)    |@| 
        field[String]("cty", (('office ! obj) andThen ('address ? obj))(js)) |@|
        field[Address]((('office ! obj) andThen ('address ! obj)), js)) { Contact }.fail.toOption.get.list should equal(List("field FirstName not found", "field cty not found"))
    }

    it("should serialize monadically") {
      // reader monad
      val contact =
        for {
          last    <- field_c[String]("lastName")
          first   <- field_c[String]("firstName")
          address <- field_c[Address]("address")
          office  <- field_c[Address]((('office ! obj) andThen ('address ! obj)))
        }
        yield(last |@| first |@| address |@| office)

      // city needs to be parsed separately since we are working on part of js
      val city = field_c[String]("city")

      // compose everything and build a Contact
      (contact(js) |@| city((('office ! obj) andThen ('address ? obj))(js))) { (last, first, address, office, city) => 
        Contact(last, first, address, city, office) } should equal(c.success)
    }
  }

  /**
  describe("Serialization of Maps") {
    it ("should serialize Map of Strings & Strings") {
      val m = Map("100" -> "dg", "200" -> "mc")
      fromjson[Map[String, String]](tojson(m)) should equal(m)
    }
  }

  describe("Serialization of composite objects") {
    it("should serialize into json and back") {
      import Protocols._
      val contact = Contact("Debasish Ghosh", 
        List(Address("monroe st", "denver", "80231"), Address("pine drive", "santa clara", "95054")))
      fromjson[Contact](tojson(contact)) should equal(contact)
    }
  }

  describe("Serialization of composite objects with arrays") {
    it("should serialize into json and back") {
      import Protocols._
      val account = Account("123", "Debasish Ghosh", 
        Array(Address("monroe st", "denver", "80231"), Address("pine drive", "santa clara", "95054")))

      val ac = fromjson[Account](tojson(account))
      ac.no should equal(account.no)
      ac.name should equal(account.name)
      ac.addresses should be === account.addresses
    }
  }
  **/

  describe("Serialization of Option") {
    it("should serialize an option field") {
      val str = Some("debasish")
      fromjson[Option[String]](tojson[Option[String]](str)) should equal(str.success)
      fromjson[Option[String]](tojson[Option[String]](None)) should equal(None.success)

      val i = Some(200)
      fromjson[Option[Int]](tojson[Option[Int]](i)) should equal(i.success)
    }
    /**
    it("should serialize AddressWithOptionalCity") {
      import TestBeans._
      import Protocols._
      val ad = AddressWithOptionalCity("garer math", Some("mumbai"), "400087")
      fromjson[AddressWithOptionalCity](tojson(ad)) should equal(ad)
    }
    it("should serialize AddressWithOptionalCity without city") {
      import TestBeans._
      import Protocols._
      val ad = AddressWithOptionalCity("garer math", None, "400087")
      fromjson[AddressWithOptionalCity](tojson(ad)) should equal(ad)
    }
    **/
  }

  describe("Serialization of tuples") {
    it("should serialize tuples of primitive types") {
      val t1 = ("debasish", 12)
      fromjson[Tuple2[String, Int]](tojson(t1)) should equal(t1.success)
      val t2 = ("debasish", 12, "jonas")
      fromjson[Tuple3[String, Int, String]](tojson(t2)) should equal(t2.success)
    }
    it("should serialize tuples of user defined types") {
      import Protocols._
      import AddressProtocol._
      val t1 = ("debasish", Address(102, "monroe st", "denver", "80231"))
      fromjson[Tuple2[String, Address]](tojson[Tuple2[String, Address]](t1)) should equal(t1.success)
    }
  }

  /**
  describe("Serialization of mutable sets") {
    it("should serialize mutable sets of primitive types") {
      import scala.collection._
      val s = mutable.Set(12, 13, 10, 23, 25)
      fromjson[mutable.Set[Int]](tojson(s)) should equal(s)
    }
    it("should serialize mutable sets of addresses") {
      import scala.collection._
      import Protocols._

      val s = mutable.Set(Address("monroe st", "denver", "80231"), Address("tamarac st", "boulder", "80231"))
      fromjson[mutable.Set[Address]](tojson(s)) should equal(s)
    }
    it("should serialize mutable sets of custom data types") {
      import scala.collection._
      import Protocols._

      val s = mutable.Set(
        ("debasish", Address("monroe st", "denver", "80231")), 
        ("maulindu", Address("tamarac st", "boulder", "80231")))
      fromjson[mutable.Set[(String, Address)]](tojson(s)) should equal(s)
    }
  }

  describe("Serialization of immutable sets") {
    it("should serialize immutable sets of primitive types") {
      import scala.collection._
      val s = immutable.Set(12, 13, 10, 23, 25)
      fromjson[immutable.Set[Int]](tojson(s)) should equal(s)
    }
    it("should serialize immutable sets of addresses") {
      import scala.collection._
      import Protocols._

      val s = immutable.Set(Address("monroe st", "denver", "80231"), Address("tamarac st", "boulder", "80231"))
      fromjson[immutable.Set[Address]](tojson(s)) should equal(s)
    }
    it("should serialize immutable sets of custom data types") {
      import scala.collection._
      import Protocols._

      val s = immutable.Set(
        ("debasish", Address("monroe st", "denver", "80231")), 
        ("maulindu", Address("tamarac st", "boulder", "80231")))
      fromjson[immutable.Set[(String, Address)]](tojson(s)) should equal(s)
    }
  }

  describe("Serialization of complex types") {
    it("should serialize complex types") {
      val l = List(Map("1"->"dg", "2"->"mc"), Map("1"->"irc", "2"->"rc", "3"->"nd"))
      fromjson[List[Map[String, String]]](tojson(l)) should equal(l)
    }
  }

  describe("Serialization of wrappers") {
    it("should serialize") {
      import Protocols._
      val n = Name("debasish ghosh")
      fromjson[Name](tojson(n)) should equal(n)
    }
    it("should serialize list wrappers") {
      import Protocols._
      val n = Holder(List("debasish ghosh", "jonas boner", "stephan schmidt"))
      fromjson[Holder](tojson(n)) should equal(n)
    }
  }

  describe("Serialization with inheritance") {
    it("should serialize") {
      import Protocols._
      import DerivedProtocol._
      val sa = new Derived("123", "debasish ghosh", Array(Address("monroe st", "denver", "80231"), Address("tamarac st", "boulder", "80231")), true)
      val acc = fromjson[Derived](tojson(sa))
      acc.no should equal(sa.no)
      acc.name should equal(sa.name)
      acc.specialFlag should equal(sa.specialFlag)
      acc.addresses should be === sa.addresses
    }
  }

  describe("Serialization with case objects") {
    it("should serialize") {
      import Protocols._
      val h = Http("http://www.google.com", Get)
      val h1 = fromjson[Http](tojson(h))
      h1 should equal(h)
    }
  }

  describe("Serialization of mutually recursive types") {
    it("should serialize without recursion") {
      import Protocols._
      val f = Foo("testFoo", List(Bar("test1", None), Bar("test2", None)))
      fromjson[Foo](tojson(f)) should equal(f)
    }
    it("should serialize with recursion") {
      import Protocols._
      val fBar = Foo("testFoo", List(Bar("test1", Some(List(Foo("barList", List(Bar("test", None), Bar("test2", None))))))))
      fromjson[Foo](tojson(fBar)) should equal(fBar)
    }
  }
  **/
}
