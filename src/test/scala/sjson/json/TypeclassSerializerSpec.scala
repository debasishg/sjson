package sjson
package json

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TypeclassSerializerSpec extends Spec with ShouldMatchers {

  import DefaultProtocol._
  import JsonSerialization._
  import Protocols._

  describe("Serialization using verbose protocol") {
    it ("should serialize a Person") {
      val p = Person("ghosh", "debasish", 20)
      fromjson[Person](tojson[Person](p)) should equal(p)
    }
  }

  describe("Serialization of simple objects") {
    it("should serialize into json and back") {
      val shop = Shop("Shoppers Stop", "dress material", 1000)
      fromjson[Shop](tojson(shop)) should equal(shop)
    }
  }

  describe("Serialization of lists") {
    it ("should serialize list of Ints") {
      val l1 = List(100, 200, 300, 400)
      fromjson[List[Int]](tojson(l1)) should equal(l1)
    }

    it ("should serialize list of Strings") {
      val l2 = List("dg", "mc", "rc", "nd")
      fromjson[List[String]](tojson(l2)) should equal(l2)
    }
  }

  describe("Serialization of Maps") {
    it ("should serialize Map of Strings & Strings") {
      val m = Map("100" -> "dg", "200" -> "mc")
      fromjson[Map[String, String]](tojson(m)) should equal(m)
    }
  }

  describe("Serialization of composite objects") {
    it("should serialize into json and back") {
      val contact = Contact("Debasish Ghosh", 
        List(Address("monroe st", "denver", "80231"), Address("pine drive", "santa clara", "95054")))
      fromjson[Contact](tojson(contact)) should equal(contact)
    }
  }

  describe("Serialization of composite objects with arrays") {
    it("should serialize into json and back") {
      val account = Account("123", "Debasish Ghosh", 
        Array(Address("monroe st", "denver", "80231"), Address("pine drive", "santa clara", "95054")))

      val ac = fromjson[Account](tojson(account))
      ac.no should equal(account.no)
      ac.name should equal(account.name)
      ac.addresses should be === account.addresses
    }
  }

  describe("Serialization of Option") {
    it("should serialize an option field") {
      val str = Some("debasish")
      fromjson[Option[String]](tojson[Option[String]](str)) should equal(str)
      fromjson[Option[String]](tojson[Option[String]](None)) should equal(None)

      val i = Some(200)
      fromjson[Option[Int]](tojson[Option[Int]](i)) should equal(i)
    }
    it("should serialize AddressWithOptionalCity") {
      import TestBeans._
      val ad = AddressWithOptionalCity("garer math", Some("mumbai"), "400087")
      fromjson[AddressWithOptionalCity](tojson(ad)) should equal(ad)
    }
    it("should serialize AddressWithOptionalCity without city") {
      import TestBeans._
      val ad = AddressWithOptionalCity("garer math", None, "400087")
      fromjson[AddressWithOptionalCity](tojson(ad)) should equal(ad)
    }
    it("should serialize None properly") {
      import TestBeans._
      val str = None
      fromjson[Option[String]](tojson[Option[String]](str)) should equal(str)
    }
    it("should serialize P that contains an Option with default value as None") {
      val p = P("ghosh", "debasish")
      fromjson[P](tojson[P](p)) should equal(p)
    }
  }

  describe("Serialization of tuples") {
    it("should serialize tuples of primitive types") {
      val t1 = ("debasish", 12)
      fromjson[Tuple2[String, Int]](tojson(t1)) should equal(t1)
      val t2 = ("debasish", 12, "jonas")
      fromjson[Tuple3[String, Int, String]](tojson(t2)) should equal(t2)
    }
    it("should serialize tuples of user defined types") {
      val t1 = ("debasish", Address("monroe st", "denver", "80231"))
      fromjson[Tuple2[String, Address]](tojson[Tuple2[String, Address]](t1)) should equal(t1)
    }
  }

  describe("Serialization of mutable sets") {
    it("should serialize mutable sets of primitive types") {
      import scala.collection._
      val s = mutable.Set(12, 13, 10, 23, 25)
      fromjson[mutable.Set[Int]](tojson(s)) should equal(s)
    }
    it("should serialize mutable sets of addresses") {
      import scala.collection._

      val s = mutable.Set(Address("monroe st", "denver", "80231"), Address("tamarac st", "boulder", "80231"))
      fromjson[mutable.Set[Address]](tojson(s)) should equal(s)
    }
    it("should serialize mutable sets of custom data types") {
      import scala.collection._

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

      val s = immutable.Set(Address("monroe st", "denver", "80231"), Address("tamarac st", "boulder", "80231"))
      fromjson[immutable.Set[Address]](tojson(s)) should equal(s)
    }
    it("should serialize immutable sets of custom data types") {
      import scala.collection._

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
      val n = Name("debasish ghosh")
      fromjson[Name](tojson(n)) should equal(n)
    }
    it("should serialize list wrappers") {
      val n = Holder(List("debasish ghosh", "jonas boner", "stephan schmidt"))
      fromjson[Holder](tojson(n)) should equal(n)
    }
  }

  describe("Serialization with inheritance") {
    it("should serialize") {
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
      val h = Http("http://www.google.com", Get)
      val h1 = fromjson[Http](tojson(h))
      h1 should equal(h)
    }
  }

  describe("Serialization of mutually recursive types") {
    it("should serialize without recursion") {
      val f = Foo("testFoo", List(Bar("test1", None), Bar("test2", None)))
      fromjson[Foo](tojson(f)) should equal(f)
    }
    it("should serialize with recursion") {
      val fBar = Foo("testFoo", List(Bar("test1", Some(List(Foo("barList", List(Bar("test", None), Bar("test2", None))))))))
      fromjson[Foo](tojson(fBar)) should equal(fBar)
    }
  }

  describe("Serialization of enumerated values") {
    it("should serialize") {
      import TestBeans._
      val js = JobStart("Debasish", WeekDay.Mon)
      fromjson[JobStart](tojson(js)) should equal(js)
    }
  }

  describe("Serialization of list of custom objects") {
    it ("should serialize list of objects") {
      val persons = List(
        Person("ghosh", "debasish", 20),
        Person("chatterjee", "maulindu", 23),
        Person("roy", "kuku", 25))

      fromjson[List[Person]](tojson(persons)) should equal(persons)
    }
  }

  describe("Serialization of double NaN") {
    it ("should serialize") {
      val x = DoubleNanTest(scala.Double.NaN)
      x.price.isNaN should equal(true)
      fromjson[DoubleNanTest](tojson(x)).price.isNaN should equal(true)
    }
  }

  describe("Serialization of list of traits with inheritance and recursive types") {
    val e1 = Employee("Mary G.", 1000)
    val e2 = Employee("John P.", 5000)
    val e3 = Employee("Peter P.", 5000)

    val d1 = Dept("Finance", e1, List(e1, e2))
    val d2 = Dept("Accounting", e2, List(e1, e2, e3))
    val d3 = Dept("Systems", e2, List(d1, d2))

    it ("should serialize") {
      fromjson[Dept](tojson(d1)) should equal(d1)
      fromjson[Dept](tojson(d3)) should equal(d3)
    }
  }
}
