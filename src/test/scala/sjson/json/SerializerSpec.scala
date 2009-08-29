package sjson.json

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scala.reflect.BeanInfo

import TestBeans._

class SerializerSpec extends Spec with ShouldMatchers {

  import dispatch.json._
  import Js._
  import Implicits._

  private[this] val serializer = Serializer.SJSON

  describe("String serialization") {
    it("should give an instance of JsString") {
      serializer.in[AnyRef](
        serializer.out("debasish ghosh")).asInstanceOf[JsValue].self.asInstanceOf[String] should equal("debasish ghosh")
    }
    it("should give an instance of String") {
      serializer.in[String](
        serializer.out("debasish ghosh")) should equal("debasish ghosh")
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
                .map(x => JsBean.fromJSON(x.asInstanceOf[JsValue], Some(classOf[BigDecimal])))
      l should equal(List(1, 2, 3))
    }
    it("should give an instance of JsArray containing JsString") {
      val l = serializer.in[AnyRef](serializer.out(List("ab", "bc", "cd")))
                .asInstanceOf[JsValue].self
                .asInstanceOf[List[_]]
                .map(x => JsBean.fromJSON(x.asInstanceOf[JsValue], Some(classOf[String])))
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
                .map(x => JsBean.fromJSON(x.asInstanceOf[JsValue], Some(classOf[String])))
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
        (JsBean.fromJSON(x._1.asInstanceOf[JsValue], Some(classOf[String])), 
          JsBean.fromJSON(x._2.asInstanceOf[JsValue], Some(classOf[String])))}.toList: _*)

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
      val address = JsBean.fromJSON(_raddr, Some(classOf[Address]))
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
                  .map(x => JsBean.fromJSON(x.asInstanceOf[JsValue], Some(classOf[Address]))))
    }
  }

  describe("Inherited bean serialization") {
    val addr = InternationalAddress("Market Street", "San Francisco", "956871", "USA")
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
}
