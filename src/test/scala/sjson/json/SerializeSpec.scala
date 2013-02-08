package sjson
package json

import java.util.TimeZone
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import TestBeans._
import Serialize._

@RunWith(classOf[JUnitRunner])
class SerializeSpec extends FunSpec with ShouldMatchers {

  import dispatch.classic.json._
  import Js._

  describe("String serialization") {
    it("should give an instance of String") {
      in[String](
        out("debasish ghosh")) should equal("debasish ghosh")
    }
    it("should give a null instance of String") {
      in[String](
        out(null)) should equal(null)
      in[String](
        out("null")) should equal("null")
    }
  }

  describe("Number serialization") {
    it("should give an instance of BigDecimal") {
      in[BigDecimal](
        out(BigDecimal(20))) should equal(BigDecimal(20))

      in[BigDecimal](
        out(BigDecimal(20))) should equal(20)
    }
  }

  describe("List serialization") {
    it("should give an instance of List") {
      val l = in[List[Int]](out(List(1, 2, 3)))
      l should equal(List(1, 2, 3))
    }

    it("should give an instance of List of String") {
      val l = in[List[String]](out(List("ab", "bc", "cd")))
      l should equal(List("ab", "bc", "cd"))
    }
  }

  describe("Map serialization") {
    it("should serialize to an instance of Map") {
      val l = Map("1" -> "debasish", "2" -> "maulindu", "3" -> "nilanjan")
      val i = in[Map[String, String]](out(l)) 
      i should equal(l)
    }
  }

  describe("Simple bean serialization") {
    val addr = Address("Market Street", "San Francisco", "956871")
    it("should give an instance of Address") {
      in[Address](
        out(addr)) should equal(addr)
    }
  }

  describe("Simple bean with null objects serialization") {
    val addr = Address("Market Street", "San Francisco", null)

    it("should give an instance of Address") {
      in[Address](
        out(addr)) should equal(addr)
    }
  }

  describe("Bean with embedded object serialization") {
    val a1 = Address("Market Street", "San Francisco", "956871")
    val a2 = Address("Monroe Street", "Denver", "80231")
    val a3 = Address("North Street", "Atlanta", "987671")
    val c = Contact("Bob", Map("residence" -> a1, "office" -> a2, "club" -> a3))
    val co = out(c)

    it("should give an instance of Contact") {
      c should equal(in[Contact](co))
    }
  }

  describe("Bean with optional simple member serialization") {
    it("should serialize with Option defined") {
      val addrCity = AddressWithOptionalCity("garer math", Some("kolkata"), "700075")
      addrCity should equal(
        in[AddressWithOptionalCity](out(addrCity)))
    }

    it("should serialize with Option not defined") {
      val addrNoCity = AddressWithOptionalCity("garer math", None, "700075")
      addrNoCity should equal(
        in[AddressWithOptionalCity](out(addrNoCity)))
    }
  }

  describe("Bean with optional bean member serialization") {
    it("should serialize with Option defined") {
      val c = new ContactWithOptionalAddr("Debasish Ghosh",
        Some(Map("primary" -> new Address("10 Market Street", "San Francisco, CA", "94111"),
            "secondary" -> new Address("3300 Tamarac Drive", "Denver, CO", "98301"))))
      c should equal(
        in[ContactWithOptionalAddr](out(c)))
    }
  }

  describe("Bean List serialization") {
    it("should serialize bean list and return a List of Address") {
      val a1 = Address("Market Street", "San Francisco", "956871")
      val a2 = Address("Monroe Street", "Denver", "80231")
      val a3 = Address("North Street", "Atlanta", "987671")
      val i = in[List[Address]](out(List(a1, a2, a3)))
      i should equal(List(a1, a2, a3))
    }
  }

  describe("Inherited bean serialization") {
    val addr = new InternationalAddress("Market Street", "San Francisco", "956871", "USA")
    it("should give an instance of the derived class InternationalAddress") {
      val ad = in[InternationalAddress](
          out(addr))

      addr.country should equal(ad.country)
      addr.city should equal(ad.city)
      addr.street should equal(ad.street)
      addr.zip should equal(ad.zip)
    }
    it("should serialize a slice and give an instance of Address") {
      val a3 = in[Address](
        out(addr))
      a3.city should equal(addr.city)
      a3.street should equal(addr.street)
      a3.zip should equal(addr.zip)

      new Address(addr.street, addr.city, addr.zip) should equal(
        in[Address](
          out(addr)))
    }
  }

  describe("Serialization with ignore properties") {
    it("should ignore issn field") {
      val j = Journal(100, "IEEE Computer", "Alex Payne", "012-456372")
      in[Journal](out(j)).issn should equal(null)
    }
  }

  describe("Serialization of Boolean") {
    it("should serialize properly") {
      val b = Foo("debasish", true)
      in[Foo](out(b)) should equal(b)
    }
  }

  describe("Serialization of vals") {
    it("should serialize properly") {
      val b = Bar("debasish", 12, 100l, 123.65f, true)
      in[Bar](out(b)) should equal(b)
    }
  }

  describe("Serialization of timezones") {
    it("should serialize properly") {
      val t = TimeZoneBean( TimeZone.getTimeZone( "UTC"))
      in[TimeZoneBean](out(t)) should equal(t)
    }
  }

  describe("Serialization of enumerations") {
    it("should serialize properly") {
      import WeekDay._
      import Shape._
      import Month._
      val b = EnumTest(Mon, Circle, April, List(Mon, Tue, Wed), List(February))
      val o = in[EnumTest](out(b))
      b.start should equal(o.start)
      b.shape should equal(o.shape)
      b.month should equal(o.month)
      b.work should equal(o.work)
      b.months should equal(o.months)
    }
  }

  import java.util.Date
  describe("Serialization of date") {
    it("should serialize properly") {
      val t = SecurityTrade("T-123", new Date, new Date, 1000)
      in[SecurityTrade](out(t)) should equal(t)
    }
  }

  describe("Serialization of tuple") {
    it("should serialize tuple2[string, string]") {
      val m = ("debasish", "ghosh")
      val i = in[Tuple2[String, String]](out(m))
      i should equal(m)
    }
    it("should serialize tuple2[string, Int]") {
      val m = ("debasish", 1000)
      val i = in[Tuple2[String, Int]](out(m))
      i should equal(m)

    }
    it("should serialize tuple2[string, List]") {
      val m = ("debasish", List(1,2,3,4))
      val i = in[Tuple2[String, List[Int]]](out(m))
      i should equal(m)
    }
    it("should serialize an object with Tuple2") {
      val message = MyMessage("id", ("hello", 34))
      val json = out(message)
      val f = in[MyMessage](json)
      f should equal(message)
    }
  }

  describe("Testing Array serialization") {
    it ("should do proper Array serialization") {
      val a = ArrayTest(100, "debasish", Array("a", "b", "c"))
      val o = out(a)
      new String(o) should equal("""{"addresses":["a","b","c"],"id":100,"name":"debasish"}""")
      val i = in[ArrayTest](o)
      i.addresses should equal(Array("a", "b", "c"))
    }
  }

  describe("Testing object array serialization") {
    val a1 = Address("Market Street", "San Francisco", "956871")
    val a2 = Address("Monroe Street", "Denver", "80231")
    val a3 = Address("North Street", "Atlanta", "987671")

    it("should do proper serialization of object arrays") {
      val a = ObjectArrayTest(100, "debasish", Array(a1, a2, a3))
      val o = out(a)
      val i = in[ObjectArrayTest](o)
      i.addresses should equal(Array(a1, a2, a3))
    }
  }

  describe("Testing for an object that contains an Int within a Map, List or Tuple2") {
    it ("should serialize properly") {
      val a = MyJsonObject("debasish", Map("debasish" -> 123), List(100), 35)
      in[MyJsonObject](out(a)) should equal(a)
      val obj = in[MyJsonObject](out(a)) 
      obj should equal(a)
      // obj.m.get("debasish").get.isInstanceOf[Int] should equal(true)
      // obj.l.head.isInstanceOf[Int] should equal(true)
    }
  }

  // https://github.com/debasishg/sjson/issues#issue/20
  describe("Testing Option for None values") {
    val s = SampleConfigOption(None, List("dg", "mc"))
    it("should serialize None to []") {
      new String(out(s)) should equal("""{"names":["dg","mc"],"user":[]}""")
    }

    it("should serialize and de-serialize correctly") {
      in[SampleConfigOption](out(s)) should equal(s)
    }
  }

  describe("Java List serialization") {
    it("should give an instance of JsArray") {
      val l = new java.util.ArrayList[Int]
      l.add(1)
      l.add(2)
      l.add(3)
      import scala.collection.JavaConversions._
      val li = in[List[Int]](out(l.toList))
      li should equal(l.toList)
    }
    it("should give another instance of JsArray") {
      val l = new java.util.ArrayList[String]
      l.add("a")
      l.add("b")
      l.add("c")
      import scala.collection.JavaConversions._
      val li = in[List[String]](out(l.toList))
      li should equal(l.toList)
    }
  }

  describe("Nested List serialization") {
    it("should give an instance of List of List of Int") {
      val l = List(List(1, 2), List(2, 3, 4), List(5))
      in[List[List[Int]]](out(l)) should equal(l)
    }
    it("should give an instance of List of List of String") {
      val l = List(List("a", "b"), List("c", "d", "e"), List("f"))
      in[List[List[String]]](out(l)) should equal(l)
    }
    it("should give an instance of List of List of Address") {
      val a1 = Address("Market Street", "San Francisco", "956871")
      val a2 = Address("Monroe Street", "Denver", "80231")
      val a3 = Address("North Street", "Atlanta", "987671")
      val l = List(List(a1), List(a1, a2), List(a1, a2, a3))
      in[List[List[Address]]](out(l)) should equal(l)
    }
  }

  describe("Serialization of Long in a bean") {
    it("should serialize") {
      val m = Manager("alexander", Some(27l))
      in[Manager](out(m)) should equal(m)
    }
  }

  // issue #26
  describe("Serialization of optionally empty fields") {
    it("should serialize Family ignoring an empty children list") {
      val pair = new Family(Some(Personz("Homer", 40)), Some(Personz("Marge", 40)))
      val o = out(pair)
      println(new String(o))
      new String(o) should equal("""{"father":{"age":40,"name":"Homer"},"mother":{"age":40,"name":"Marge"}}""")
      in[Family](o) should equal(pair)
    }

    it("should serialize a list of orphans without the empty parents in output") {
      val orphan = new Family(None, None, List(Personz("Bart", 10), Personz("Lisa", 8)))
      val o = out(orphan)
      new String(o) should equal("""{"children":[{"age":10,"name":"Bart"},{"age":8,"name":"Lisa"}]}""")
      in[Family](o) should equal(orphan)
    }
  }

  describe("Serialization of classes containing singleton objects") {
    it("should serialize BondTrade with FI type of security") {
      val sec = Security("Google", FI)
      val fitrade = BondTrade(sec, Some(Map("TradeTax" -> BigDecimal(10.00), "Commission" -> BigDecimal(23))), "a-123")
      val o = out(fitrade)
      in[BondTrade](o) should equal(fitrade)
    }

    it("should serialize a BondTrade with FI type of security and empty taxfee list") {
      val sec = Security("Google", FI)
      val fitrade = BondTrade(sec, None, "a-123")
      val o = out(fitrade)
      in[BondTrade](o) should equal(fitrade)
    }
  }

  describe("Serialization of classes containing collections of singleton objects") {
    it("should serialize an object with a Map containing lists of case objects") {
      val t = TradedIn(Map("New York" -> List(STOCK, FI), "Hong Kong" -> List(STOCK, MUTUAL_FUND)))
      val o = out(t)
      in[TradedIn](o) should equal(t)
    }
  }

  describe("Serialization of classes containing Maps of Lists as values") {
    it("should serialize a Map of list of String") {
      val t = MapOfListOfString(Map("New York" -> List("FI", "STOCK"), "Hong Kong" -> List("STOCK")))
      val o = out(t)
      in[MapOfListOfString](o) should equal(t)
    }

    it("should serialize an optional Map of list of String") {
      val t = OptionalMapOfListOfString(Some(Map("New York" -> List("FI", "STOCK"), "Hong Kong" -> List("STOCK"))))
      val o = out(t)
      in[OptionalMapOfListOfString](o) should equal(t)
    }

    it("should serialize a Map of list of Shop") {
      val s1 = Shop("macys", "garment", 100)
      val s2 = Shop("sears", "shirt", 120)
      val s3 = Shop("costco", "banana", 15)
      val s4 = Shop("jcpenny", "garment", 200)

      val t = MapOfListOfShop(Map("New York" -> List(s1, s2), "Hong Kong" -> List(s2, s3, s4)))
      val o = out(t)
      in[MapOfListOfShop](o) should equal(t)
    }

    it("should serialize an optional Map of list of Shop") {
      val s1 = Shop("macys", "garment", 100)
      val s2 = Shop("sears", "shirt", 120)
      val s3 = Shop("costco", "banana", 15)
      val s4 = Shop("jcpenny", "garment", 200)

      val t = OptionalMapOfListOfShop(Some(Map("New York" -> List(s1, s2), "Hong Kong" -> List(s2, s3, s4))))
      val o = out(t)
      in[OptionalMapOfListOfShop](o) should equal(t)
    }

    it("should serialize a Map of optional list of String") {
      val t = MapOfOptionalListOfString(Map("New York" -> Some(List("FI", "STOCK")), "Hong Kong" -> Some(List("STOCK"))))
      val o = out(t)
      in[MapOfOptionalListOfString](o) should equal(t)
    }

    it("should serialize a Map of optional String") {
      val t = MapOfOptionalString(Map("New York" -> Some("STOCK"), "Hong Kong" -> Some("FI")))
      val o = out(t)
      in[MapOfOptionalString](o) should equal(t)
    }

    it("should serialize a Map of optional Shop") {
      val s1 = Shop("macys", "garment", 100)
      val s2 = Shop("sears", "shirt", 120)
      val t = MapOfOptionalShop(Map("New York" -> Some(s1), "Hong Kong" -> Some(s2)))
      val o = out(t)
      in[MapOfOptionalShop](o) should equal(t)
    }

    it("should serialize a List of Map") {
      val s1 = Shop("macys", "garment", 100)
      val s2 = Shop("sears", "shirt", 120)
      val t = ListOfMap(List(Map("New York" -> s1, "Hong Kong" -> s2)))
      val o = out(t)
      in[ListOfMap](o) should equal(t)
    }

    it("should serialize a List of Map of String") {
      val t = ListOfMapOfString(List(Map("New York" -> "US", "Kolkata" -> "India")))
      val o = out(t)
      in[ListOfMapOfString](o) should equal(t)
    }

    it("should serialize a List of Map of optional String") {
      val t = ListOfMapOfOptionalString(List(Map("New York" -> Some("US"), "Kolkata" -> Some("India"))))
      val o = out(t)
      in[ListOfMapOfOptionalString](o) should equal(t)
    }
  }

  describe("String with $") {
    it ("should serialize with trailing $") {
      val u = User("dghosh@acm.org", "123abc$")
      in[User](out(u)) should equal(u)
    }
    it ("should serialize with middle $") {
      val u = User("dghosh@acm.org", "123abc$xyz")
      in[User](out(u)) should equal(u)
    }
    it ("should serialize with beginning $") {
      val u = User("dghosh@acm.org", "$123abc")
      in[User](out(u)) should equal(u)
    }
    it ("should serialize with multiple $") {
      val u = User("dghosh@acm.org", "$123abc$xyz$")
      in[User](out(u)) should equal(u)
    }
  }
}
