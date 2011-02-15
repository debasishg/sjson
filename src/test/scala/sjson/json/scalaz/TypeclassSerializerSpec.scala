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

  describe("Serialization using standard protocol") {
    it ("should serialize a Person") {
      import Protocols._
      import PersonProtocol._
      val a = Address(12, "Monroe Street", "Denver", "80231")
      val p = Person("ghosh", "debasish", 20, a)
      fromjson[Person](tojson(p)) should equal(p.success)
    }

    it ("should serialize an Address") {
      import Protocols._
      import AddressProtocol._
      val a = Address(12, "Tamarac Square", "Denver", "80231")
      fromjson[Address](tojson(a)) should equal(a.success)
    }
  }

  describe("Serialization using incorrect protocol") {
    it ("address serialization should fail") {
      import Protocols._
      import IncorrectAddressProtocol._
      val a = Address(12, "Monroe Street", "Denver", "80231")
      (fromjson[Address](tojson(a))).fail.toOption.get.list should equal (List("field number not found", "field stret not found", "field City not found"))
    }
    it ("person serialization should fail") {
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

  describe("Serialization of simple objects") {
    it("should serialize into json and back") {
      import Protocols._
      val shop = Shop("Shoppers Stop", "dress material", 1000)
      fromjson[Shop](tojson(shop)) should equal(shop.success)
    }
  }

  describe("Serialization of Maps") {
    it ("should serialize Map of Strings & Strings") {
      val m = Map("100" -> "dg", "200" -> "mc")
      fromjson[Map[String, String]](tojson(m)) should equal(m.success)
    }
  }

  describe("Serialization of composite objects") {
    it("should serialize into json and back") {
      import Protocols._
      val addressBook = AddressBook("Debasish Ghosh", 
        List(Address(100, "monroe st", "denver", "80231"), Address(23, "pine drive", "santa clara", "95054")))
      fromjson[AddressBook](tojson(addressBook)) should equal(addressBook.success)
    }
  }

  describe("Serialization of composite objects with arrays") {
    it("should serialize into json and back") {
      import Protocols._
      val account = Account("123", "Debasish Ghosh", 
        Array(Address(100, "monroe st", "denver", "80231"), Address(234, "pine drive", "santa clara", "95054")))

      val Success(ac) = fromjson[Account](tojson(account))
      ac.no should equal(account.no)
      ac.name should equal(account.name)
      ac.addresses should be === account.addresses
    }
  }

  describe("Serialization of Option") {
    it("should serialize an option field") {
      val str = Some("debasish")
      fromjson[Option[String]](tojson[Option[String]](str)) should equal(str.success)
      fromjson[Option[String]](tojson[Option[String]](None)) should equal(None.success)

      val i = Some(200)
      fromjson[Option[Int]](tojson[Option[Int]](i)) should equal(i.success)
    }
    it("should serialize AddressWithOptionalCity") {
      import sjson.json.TestBeans._
      import Protocols._
      val ad = AddressWithOptionalCity("garer math", Some("mumbai"), "400087")
      fromjson[AddressWithOptionalCity](tojson(ad)) should equal(ad.success)
    }
    it("should serialize AddressWithOptionalCity without city") {
      import sjson.json.TestBeans._
      import Protocols._
      val ad = AddressWithOptionalCity("garer math", None, "400087")
      fromjson[AddressWithOptionalCity](tojson(ad)) should equal(ad.success)
    }
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

  describe("Serialization of mutable sets") {
    it("should serialize mutable sets of primitive types") {
      import scala.collection._
      val s = mutable.Set(12, 13, 10, 23, 25)
      fromjson[mutable.Set[Int]](tojson(s)) should equal(s.success)
    }
    it("should serialize mutable sets of addresses") {
      import scala.collection._
      import Protocols._

      val s = mutable.Set(Address(100, "monroe st", "denver", "80231"), Address(23, "tamarac st", "boulder", "80231"))
      fromjson[mutable.Set[Address]](tojson(s)) should equal(s.success)
    }
    it("should serialize mutable sets of custom data types") {
      import scala.collection._
      import Protocols._

      val s = mutable.Set(
        ("debasish", Address(100, "monroe st", "denver", "80231")), 
        ("maulindu", Address(23, "tamarac st", "boulder", "80231")))
      fromjson[mutable.Set[(String, Address)]](tojson(s)) should equal(s.success)
    }
  }

  describe("Serialization of immutable sets") {
    it("should serialize immutable sets of primitive types") {
      import scala.collection._
      val s = immutable.Set(12, 13, 10, 23, 25)
      fromjson[immutable.Set[Int]](tojson(s)) should equal(s.success)
    }
    it("should serialize immutable sets of addresses") {
      import scala.collection._
      import Protocols._

      val s = immutable.Set(Address(100, "monroe st", "denver", "80231"), Address(23, "tamarac st", "boulder", "80231"))
      fromjson[immutable.Set[Address]](tojson(s)) should equal(s.success)
    }
    it("should serialize immutable sets of custom data types") {
      import scala.collection._
      import Protocols._

      val s = immutable.Set(
        ("debasish", Address(100, "monroe st", "denver", "80231")), 
        ("maulindu", Address(23, "tamarac st", "boulder", "80231")))
      fromjson[immutable.Set[(String, Address)]](tojson(s)) should equal(s.success)
    }
  }

  describe("Serialization of complex types") {
    it("should serialize complex types") {
      val l = List(Map("1"->"dg", "2"->"mc"), Map("1"->"irc", "2"->"rc", "3"->"nd"))
      fromjson[List[Map[String, String]]](tojson(l)) should equal(l.success)
    }
  }

  describe("Serialization of wrappers") {
    it("should serialize") {
      import Protocols._
      val n = Name("debasish ghosh")
      fromjson[Name](tojson(n)) should equal(n.success)
    }
    it("should serialize list wrappers") {
      import Protocols._
      val n = Holder(List("debasish ghosh", "jonas boner", "stephan schmidt"))
      fromjson[Holder](tojson(n)) should equal(n.success)
    }
  }

  describe("Serialization with inheritance") {
    it("should serialize") {
      import Protocols._
      import DerivedProtocol._
      val sa = new Derived("123", "debasish ghosh", List(Address(100, "monroe st", "denver", "80231"), Address(23, "tamarac st", "boulder", "80231")), true)
      val Success(acc) = fromjson[Derived](tojson(sa))
      acc should equal(sa)
    }
  }

  describe("Serialization with case objects") {
    it("should serialize") {
      import Protocols._
      val h = Http("http://www.google.com", Get)
      val h1 = fromjson[Http](tojson(h))
      h1 should equal(h.success)
    }
  }

  describe("Serialization of mutually recursive types") {
    it("should serialize without recursion") {
      import Protocols._
      val f = Foo("testFoo", List(Bar("test1", None), Bar("test2", None)))
      fromjson[Foo](tojson(f)) should equal(f.success)
    }
    it("should serialize with recursion") {
      import Protocols._
      val fBar = Foo("testFoo", List(Bar("test1", Some(List(Foo("barList", List(Bar("test", None), Bar("test2", None))))))))
      fromjson[Foo](tojson(fBar)) should equal(fBar.success)
    }
  }

  describe("Serialization followed by composition into objects") {
    it ("should serialize an Address and name and compose") {
      import Protocols._
      import AddressProtocol._

      import dispatch.json._
      import Js._

      val nameJsonString = """{"lastName" : "ghosh", "firstName" : "debasish"}""" 
      val addressJsonString = """{"no" : 12, "street" : "Tamarac Square", "city" : "Denver", "zip" : "80231" }""" 

      val njs = Js(nameJsonString)
      val ajs = Js(addressJsonString)
      val a = Address(12, "Tamarac Square", "Denver", "80231")

      val n = GoodName("ghosh", "debasish")

      // reader monad
      val address =
        for {
          no      <- field_c[Int]("no")
          street  <- field_c[String]("street")
          city    <- field_c[String]("city")
          zip     <- field_c[String]("zip")
        }
        yield(no |@| street |@| city |@| zip)

      val name = 
        for {
          ln <- field_c[String]("lastName")
          fn <- field_c[String]("firstName")
        }
        yield(ln |@| fn)

      name(njs) { GoodName } should equal(n.success)
      address(ajs) { Address } should equal(a.success)

      case class Foo(name: GoodName, address: Address)
      println((address(ajs) { Address } |@| name(njs) { GoodName }) { (a, n) => Foo(n, a) })
      (address(ajs) { Address } |@| name(njs) { GoodName }) { (a, n) => Foo(n, a) } should equal(Foo(n, a).success)
    }
  }
}
