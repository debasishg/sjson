package sjson
package json

import java.util.TimeZone
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.reflect.BeanInfo

import TestBeans._

@RunWith(classOf[JUnitRunner])
class SerializerSpec extends Spec with ShouldMatchers {

  import dispatch.json._
  import Js._
  import Implicits._

  private[this] val serializer = Serializer.SJSON

  val jsBean = new Object with JsBean with DefaultConstructor

  describe("String serialization") {
    it("should give an instance of JsString") {
      serializer.in[AnyRef](
        serializer.out("debasish ghosh")).asInstanceOf[JsValue].self.asInstanceOf[String] should equal("debasish ghosh")
    }
    it("should give an instance of String") {
      serializer.in[String](
        serializer.out("debasish ghosh")) should equal("debasish ghosh")
    }
    it("should give a null instance of String") {
      serializer.in[String](
        serializer.out(null)) should equal(null)
      serializer.in[String](
        serializer.out("null")) should equal("null")
    }
  }

  describe("Number serialization") {
    it("should give an instance of JsNumber") {
      val JsNumber(n) = serializer.in[AnyRef](serializer.out(BigDecimal(20))).asInstanceOf[JsValue]
      n should equal(20)

      val JsNumber(n1) = serializer.in[AnyRef](serializer.out(BigInt(20))).asInstanceOf[JsValue]
      n1 should equal(20)
    }
    it("should give an instance of BigDecimal") {
      serializer.in[BigDecimal](
        serializer.out(BigDecimal(20))) should equal(BigDecimal(20))

      serializer.in[BigDecimal](
        serializer.out(BigDecimal(20))) should equal(20)
    }
  }

  describe("List serialization") {
    it("should give an instance of JsArray") {
      val l = serializer.in[AnyRef](serializer.out(List(1, 2, 3))).asInstanceOf[JsValue]
      val num_list = list ! num
      val num_list(l0) = l
      l0 should equal(List(1, 2, 3))
    }
    it("should give an instance of List") {
      val l = serializer.in[List[BigDecimal]](serializer.out(List(1, 2, 3)))
                .asInstanceOf[List[_]]
                .map(x => jsBean.fromJSON(x.asInstanceOf[JsValue], Some(classOf[BigDecimal])))
      l should equal(List(1, 2, 3))
    }
    it("should give an instance of JsArray containing JsString") {
      val l = serializer.in[AnyRef](serializer.out(List("ab", "bc", "cd")))
                .asInstanceOf[JsValue].self
                .asInstanceOf[List[_]]
                .map(x => jsBean.fromJSON(x.asInstanceOf[JsValue], Some(classOf[String])))
      l should equal(List("ab", "bc", "cd"))
    }

    // use list extractor
    it("should also give a List using extractors") {
      val l = serializer.in[AnyRef](serializer.out(List("ab", "bc", "cd")))
      val str_list = list ! str
      val str_list(l0) = l
      l0 should equal(List("ab", "bc", "cd"))
    }
    it("should give an instance of List of String") {
      val l = serializer.in[List[String]](serializer.out(List("ab", "bc", "cd")))
                .asInstanceOf[List[_]]
                .map(x => jsBean.fromJSON(x.asInstanceOf[JsValue], Some(classOf[String])))
      l should equal(List("ab", "bc", "cd"))
    }
  }

  describe("Map serialization") {
    it("should serialize to an instance of Map") {
      val m = Map("1" -> "dg", "2" -> "mc", "3" -> "rc")
      val m1 = serializer.in[AnyRef](serializer.out(m))
                         .asInstanceOf[JsValue].self
                         .asInstanceOf[Map[_,_]]
      val m2 = Map(m1.map {x => 
        (jsBean.fromJSON(x._1.asInstanceOf[JsValue], Some(classOf[String])), 
          jsBean.fromJSON(x._2.asInstanceOf[JsValue], Some(classOf[String])))}.toList: _*)

      m.keySet.toList should equal(m2.keySet.toList)
    }
  }
  
  describe("Simple bean serialization") {
    val addr = Address("Market Street", "San Francisco", "956871")
    it("should give an instance of Address") {
      serializer.in[Address](
        serializer.out(addr)) should equal(addr)
    }

    it("should match extracted values") {
      val a = serializer.in[AnyRef](
        serializer.out(addr))

      // use extractors
      val c = 'city ? str
      val c(_city) = a
      _city should equal("San Francisco")

      val s = 'street ? str
      val s(_street) = a
      _street should equal("Market Street")

      val z = 'zip ? str
      val z(_zip) = a
      _zip should equal("956871")
    }
  }

  describe("Simple bean with null objects serialization") {
    val addr = Address("Market Street", "San Francisco", null)

    it("should give an instance of Address") {
      serializer.in[Address](
        serializer.out(addr)) should equal(addr)
    }

    it("should match extracted values") {
      val a = serializer.in[AnyRef](
        serializer.out(addr))

      // use extractors
      val c = 'city ? str
      val c(_city) = a
      _city should equal("San Francisco")

      val s = 'street ? str
      val s(_street) = a
      _street should equal("Market Street")

      val z = 'zip ? str
      // println("z = " + z)
      // val z(_zip) = a
      // _zip should equal("null") // should be JsNull since de-serialized with AnyRef
    }
  }

  describe("Bean with embedded object serialization") {
    val a1 = Address("Market Street", "San Francisco", "956871")
    val a2 = Address("Monroe Street", "Denver", "80231")
    val a3 = Address("North Street", "Atlanta", "987671")
    val c = Contact("Bob", Map("residence" -> a1, "office" -> a2, "club" -> a3))
    val co = serializer.out(c)

    it("should give an instance of Contact") {
      c should equal(serializer.in[Contact](co))
    }
    it("should match extracted values from Contact") {
      val a = serializer.in[AnyRef](co)

      // extract name
      val n = 'name ? str
      val n(_name) = a
      "Bob" should equal(_name)

      // extract addresses
      val addrs = 'addresses ? obj
      val addrs(_addresses) = a

      // extract residence from addresses
      val res = 'residence ? obj
      val res(_raddr) = _addresses

      // make an Address bean out of _raddr
      val address = jsBean.fromJSON(_raddr, Some(classOf[Address]))
      a1 should equal(address)

      object r { def ># [T](f: JsF[T]) = f(a.asInstanceOf[JsValue]) }

      // still better: chain 'em up
      "Market Street" should equal(
        (r ># { ('addresses ? obj) andThen ('residence ? obj) andThen ('street ? str) }))
    }
  }

  describe("Bean with optional simple member serialization") {
    it("should serialize with Option defined") {
      val addrCity = AddressWithOptionalCity("garer math", Some("kolkata"), "700075")
      addrCity should equal(
        serializer.in[AddressWithOptionalCity](serializer.out(addrCity)))
    }

    it("should serialize with Option not defined") {
      val addrNoCity = AddressWithOptionalCity("garer math", None, "700075")
      addrNoCity should equal(
        serializer.in[AddressWithOptionalCity](serializer.out(addrNoCity)))
    }
  }

  describe("Bean with optional bean member serialization") {
    it("should serialize with Option defined") {
      val c = new ContactWithOptionalAddr("Debasish Ghosh",
        Some(Map("primary" -> new Address("10 Market Street", "San Francisco, CA", "94111"),
            "secondary" -> new Address("3300 Tamarac Drive", "Denver, CO", "98301"))))
      c should equal(
        serializer.in[ContactWithOptionalAddr](serializer.out(c)))
    }
  }

  describe("Bean List serialization") {
    it("should serialize bean list and return a List of Address") {
      val a1 = Address("Market Street", "San Francisco", "956871")
      val a2 = Address("Monroe Street", "Denver", "80231")
      val a3 = Address("North Street", "Atlanta", "987671")
      List(a1, a2, a3) should equal(
        serializer.in[AnyRef](serializer.out(List(a1, a2, a3)))
                  .asInstanceOf[JsValue].self
                  .asInstanceOf[List[_]]
                  .map(x => jsBean.fromJSON(x.asInstanceOf[JsValue], Some(classOf[Address]))))
    }
  }

  describe("Inherited bean serialization") {
    val addr = new InternationalAddress("Market Street", "San Francisco", "956871", "USA")
    it("should give an instance of the derived class InternationalAddress") {
      val ad = serializer.in[InternationalAddress](
          serializer.out(addr)).asInstanceOf[InternationalAddress]

      addr.country should equal(ad.country)
      addr.city should equal(ad.city)
      addr.street should equal(ad.street)
      addr.zip should equal(ad.zip)
    }
    it("should serialize a slice and give an instance of Address") {
      val a3 = serializer.in[Address](
        serializer.out(addr)).asInstanceOf[Address]
      a3.city should equal(addr.city)
      a3.street should equal(addr.street)
      a3.zip should equal(addr.zip)

      new Address(addr.street, addr.city, addr.zip) should equal(
        serializer.in[Address](
          serializer.out(addr)))
    }
    it("should support deserialization as AnyRef and extraction") {
      val a = serializer.in[AnyRef](
        serializer.out(addr))

      // use extractors
      val c = 'city ? str
      val c(_city) = a
      _city should equal("San Francisco")

      val s = 'street ? str
      val s(_street) = a
      _street should equal("Market Street")

      val z = 'zip ? str
      val z(_zip) = a
      _zip should equal("956871")

      val n = 'country ? str
      val n(_cnt) = a
      _cnt should equal("USA")
    }
    it("should support anonymous deserialization and extraction") {
      val a = serializer.in(
        serializer.out(addr))

      // use extractors
      val c = 'city ? str
      val c(_city) = a
      _city should equal("San Francisco")

      val s = 'street ? str
      val s(_street) = a
      _street should equal("Market Street")

      val z = 'zip ? str
      val z(_zip) = a
      _zip should equal("956871")

      val n = 'country ? str
      val n(_cnt) = a
      _cnt should equal("USA")
    }
  }

  describe("Serialization with ignore properties") {
    it("should ignore issn field") {
      val j = Journal(100, "IEEE Computer", "Alex Payne", "012-456372")
      serializer.in[Journal](serializer.out(j)).asInstanceOf[Journal].issn should equal(null)
    }
  }

  describe("Serialization of Boolean") {
    it("should serialize properly") {
      val b = Foo("debasish", true)
      serializer.in[Foo](serializer.out(b)).asInstanceOf[Foo] should equal(b)
    }
  }

  describe("Serialization of vals") {
    it("should serialize properly") {
      val b = Bar("debasish", 12, 100l, 123.65f, true)
      serializer.in[Bar](serializer.out(b)).asInstanceOf[Bar] should equal(b)
    }
  }

  describe("Serialization of timezones") {
    it("should serialize properly") {
      val t = TimeZoneBean( TimeZone.getTimeZone( "UTC"))
      serializer.in[TimeZoneBean](serializer.out(t)).asInstanceOf[TimeZoneBean] should equal(t)
    }
  }

  describe("Serialization of enumerations") {
    it("should serialize properly") {
      // val b = EnumTest( TestEnum.One)
      import WeekDay._
      import Shape._
      val b = EnumTest(Mon, Circle, List(Mon, Tue, Wed))
      val o = serializer.in[EnumTest](serializer.out(b)).asInstanceOf[EnumTest] 
      b.start should equal(o.start)
      b.shape should equal(o.shape)
    }
  }

  import java.util.Date
  describe("Serialization of date") {
    it("should serialize properly") {
      val t = SecurityTrade("T-123", new Date, new Date, 1000)
      serializer.in[SecurityTrade](serializer.out(t)).asInstanceOf[SecurityTrade] should equal(t)
    }
  }

  describe("Serialization of tuple") {
    it("should serialize tuple2[string, string]") {
      val m = ("debasish", "ghosh")

      val m1 = serializer.in[AnyRef](serializer.out(m))
                         .asInstanceOf[JsValue].self
                         .asInstanceOf[Map[_,_]]
      val m2 = Map(m1.map {x => 
        (jsBean.fromJSON(x._1.asInstanceOf[JsValue], Some(classOf[String])), 
          jsBean.fromJSON(x._2.asInstanceOf[JsValue], Some(classOf[String])))}.toList: _*)

      Map(m).valuesIterator.toList should equal(m2.valuesIterator.toList)
    }
    it("should serialize tuple2[string, Int]") {
      val m = ("debasish", 1000)

      val m1 = serializer.in[AnyRef](serializer.out(m))
                         .asInstanceOf[JsValue].self
                         .asInstanceOf[Map[_,_]]
      val m2 = Map(m1.map {x => 
        (jsBean.fromJSON(x._1.asInstanceOf[JsValue], Some(classOf[String])), 
          jsBean.fromJSON(x._2.asInstanceOf[JsValue], Some(classOf[String])))}.toList: _*)

      Map(m).valuesIterator.toList should equal(m2.valuesIterator.toList)
    }
    it("should serialize tuple2[string, List]") {
      val m = ("debasish", List(1,2,3,4))

      val e = serializer.in(
        serializer.out(m))

      val JsObject(o) = e
      o(JsString("debasish")) should equal(JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4))))
    }
    it("should serialize an object with Tuple2") {
      val message = MyMessage("id", ("hello", 34))
      val json = serializer.out(message)
      new String(json) should equal("""{"id":"id","value":{"hello":34}}""")
      val f = serializer.in[MyMessage](json).asInstanceOf[MyMessage]
      f should equal(message)
    }
  }

  object MySJSON extends Serializer.SJSON with DefaultConstructor {
    val classLoader = Some(this.getClass.getClassLoader)
  }

  describe("Serialization with classloader API") {
    it("should serialize properly and de-serialize") {
      val s = MySJSON.out("debasish")
      MySJSON.in(s, "java.lang.String") should equal("debasish")

      val shop = Shop("Shoppers Stop", "Gold", 120)
      val sh = MySJSON.out(shop)
      val insh = MySJSON.in(sh, "sjson.json.TestBeans$Shop").asInstanceOf[Shop]
      insh.store should equal(shop.store)
      insh.item should equal(shop.item)
      insh.price should equal(shop.price)
    }
    it("should serialize an object with Tuple2") {
      val message = MyMessage("id", ("hello", 34))
      val json = new String(MySJSON.out(message))
      json should equal("""{"id":"id","value":{"hello":34}}""")
    }
  }

  object MyObjenesisSJSON extends Serializer.SJSON with Objenesis {
    val classLoader = Some(this.getClass.getClassLoader)
  }

  describe("Bean with no default constructor using Objenesis") {
    it("should serialize properly and de-serialize") {
      val s = MyObjenesisSJSON.out("joewalnes")
      MyObjenesisSJSON.in(s, "java.lang.String") should equal("joewalnes")

      val shop = ShopWithNoDefaultConstructor("Shoppers Stop", "Gold", 120)
      val sh = MyObjenesisSJSON.out(shop)
      val insh = MyObjenesisSJSON.in(sh, "sjson.json.TestBeans$ShopWithNoDefaultConstructor").asInstanceOf[ShopWithNoDefaultConstructor]
      insh.store should equal(shop.store)
      insh.item should equal(shop.item)
      insh.price should equal(shop.price)
    }
  }

  describe("Bean serialization with class loading") {
    val addr = Address("Market Street", "San Francisco", "956871")
    it("should give an instance of Address") {
      MySJSON.in(
        MySJSON.out(addr), "sjson.json.TestBeans$Address") should equal(addr)
    }
  }

  describe("Bean with optional bean member serialization with class loading") {
    it("should serialize with Option defined") {
      val c = new ContactWithOptionalAddr("Debasish Ghosh",
        Some(Map("primary" -> new Address("10 Market Street", "San Francisco, CA", "94111"),
            "secondary" -> new Address("3300 Tamarac Drive", "Denver, CO", "98301"))))
      c should equal(
        MySJSON.in(MySJSON.out(c), "sjson.json.TestBeans$ContactWithOptionalAddr"))
    }
  }

  describe("Testing Array serialization") {
    it ("should do proper Array serialization") {
      val a = ArrayTest(100, "debasish", Array("a", "b", "c"))
      val out = serializer.out(a)
      new String(out) should equal("""{"addresses":["a","b","c"],"id":100,"name":"debasish"}""")
      val in = serializer.in[ArrayTest](out)
      in.asInstanceOf[ArrayTest].addresses should equal(Array("a", "b", "c"))
    }
  }

  describe("Testing object array serialization") {
    val a1 = Address("Market Street", "San Francisco", "956871")
    val a2 = Address("Monroe Street", "Denver", "80231")
    val a3 = Address("North Street", "Atlanta", "987671")

    it("should do proper serialization of object arrays") {
      val a = ObjectArrayTest(100, "debasish", Array(a1, a2, a3))
      val out = serializer.out(a)
      val in = serializer.in[ObjectArrayTest](out)
      in.asInstanceOf[ObjectArrayTest].addresses should equal(Array(a1, a2, a3))
    }
  }

  describe("Testing for an object that contains an Int within a Map, List or Tuple2") {
    it ("should serialize properly") {
      val a = MyJsonObject("debasish", Map("debasish" -> 123), List(100), 35)
      serializer.in[MyJsonObject](serializer.out(a)) should equal(a)
      val obj = serializer.in[MyJsonObject](serializer.out(a)) 
      obj should equal(a)
      obj.asInstanceOf[MyJsonObject].m.get("debasish").get.isInstanceOf[Int] should equal(true)
      obj.asInstanceOf[MyJsonObject].l.head.isInstanceOf[Int] should equal(true)
    }
  }
}
