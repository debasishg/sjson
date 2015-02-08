package sjson
package json

import org.scalatest.FunSpec
import org.scalatest.MustMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import java.util.Calendar
import dispatch.classic.json._
import TestBeans._

@RunWith(classOf[JUnitRunner])
class JsSpec extends FunSpec with MustMatchers {
  import Jsons._

  describe("to json of a number") {
    it("should serialize to json") {
      val n1 = toJSON_n(100)
      val n2 = toJSON_n(100.23)
      val n3 = toJSON_n(BigDecimal(100))

      n1 must equal("100")
      n2 must equal("100.23")
      n3 must equal("100")
    }
  }

  describe("to json of a boolean") {
    it("should serialize to json") {
      val b1 = true
      val b2 = false
      toJSON_n(b1) must equal("true")
      toJSON_n(b2) must equal("false")
    }
  }

  describe("to json of a List") {
    it("should serialize to json") {
      // List[Int]
      val l1 = List(1,2,3,4)
      toJSON_n(l1) must equal("[1,2,3,4]")

      // List[String]
      val l2 = List("a", "b", "c", "d")
      toJSON_n(l2) must equal("""["a","b","c","d"]""")

      // List[List[String]]
      val l3 = List(List("a", "b"), List("c", "d"), List("e", "f", "g"))
      toJSON_n(l3) must equal("""[["a","b"],["c","d"],["e","f","g"]]""")

      // List[Tuple2]
      val l4 = List((1, "a"), (2, "b"), (3, "c"))
      toJSON_n(l4) must equal("""[{1:"a"},{2:"b"},{3:"c"}]""")
    }
  }

  describe("to json of a Vector") {
    it("should serialize to json") {
      // Vector[Int]
      val l1 = Vector(1,2,3,4)
      toJSON_n(l1) must equal("[1,2,3,4]")

      // Vector[String]
      val l2 = Vector("a", "b", "c", "d")
      toJSON_n(l2) must equal("""["a","b","c","d"]""")

      // Vector[List[String]]
      val l3 = Vector(List("a", "b"), List("c", "d"), List("e", "f", "g"))
      toJSON_n(l3) must equal("""[["a","b"],["c","d"],["e","f","g"]]""")

      // Vector[Tuple2]
      val l4 = Vector((1, "a"), (2, "b"), (3, "c"))
      toJSON_n(l4) must equal("""[{1:"a"},{2:"b"},{3:"c"}]""")
    }
  }

  describe("to json of a Map") {
    it("should serialize to json") {
      // Map Int -> String
      val m1 = Map(1->"a", 2->"b", 3->"c")
      toJSON_n(m1) must equal("""{"1":"a","2":"b","3":"c"}""")

      // Map Int -> Int
      val m2 = Map(1->1, 2->2, 3->3)
      toJSON_n(m2) must equal("""{"1":1,"2":2,"3":3}""")

      // Map Int -> List[Int]
      val m3 = Map(1->List(1,2,3), 2->List(4,5,6), 3->List(7,8,9))
      toJSON_n(m3) must equal("""{"1":[1,2,3],"2":[4,5,6],"3":[7,8,9]}""")

      // Map Int -> List[List[Int]]
      val m4 = Map(1->List(List(1,2),List(3)), 2->List(List(4),List(5,6)), 3->List(List(7,8,9)))
      toJSON_n(m4) must equal("""{"1":[[1,2],[3]],"2":[[4],[5,6]],"3":[[7,8,9]]}""")
    }
  }

  describe("to json of an object") {
    it("should serialize to json") {
      // simple object
      val s = Shop("kmart", "fridge", 100)
      toJSON_n(s) must equal("""{"price":100,"item":"fridge","store":"kmart"}""")

      // object with an embedded Map
      val c = Contact("debasish ghosh", Map("primary" -> Address("Tamarac Street", "Denver", "80231"), 
                                            "secondary" -> Address("Garden Street", "Cupertino", "95054")))
      toJSON_n(c) must equal("""{"addresses":{"primary":{"zip":"80231","city":"Denver","street":"Tamarac Street"},"secondary":{"zip":"95054","city":"Cupertino","street":"Garden Street"}},"name":"debasish ghosh"}""")

      // object with an optional field
      val oa1 = AddressWithOptionalCity("Tamarac Street", Some("Denver"), "80231")
      toJSON_n(oa1) must equal("""{"zip":"80231","city":"Denver","street":"Tamarac Street"}""")
      val oa2 = AddressWithOptionalCity("Tamarac Street", None, "80231")
      toJSON_n(oa2) must equal("""{"zip":"80231","city":[],"street":"Tamarac Street"}""")

      // object with an embedded List
      val p = Person("ghosh", "debasish", 
                     List(Address("Tamarac Street", "Denver", "80231"), Address("Garden Street", "Cupertino", "95054")))
      toJSON_n(p) must equal("""{"addresses":[{"zip":"80231","city":"Denver","street":"Tamarac Street"},{"zip":"95054","city":"Cupertino","street":"Garden Street"}],"firstName":"debasish","lastName":"ghosh"}""")

      // a complex object
      val addresses = List(Address("10 Market Street", "San Francisco, CA", "94111"),
                           Address("3300 Tamarac Drive", "Denver, CO", "98301"))
      val e = new Employee(100,
                           "Jason Alexander",
                           "Circuit City",
                           addresses,
                           Salary(4500, 245))
      toJSON_n(e) must equal("""{"sal":{"allowance":245,"basic":4500},"addresses":[{"zip":"94111","city":"San Francisco, CA","street":"10 Market Street"},{"zip":"98301","city":"Denver, CO","street":"3300 Tamarac Drive"}],"prevEmployer":"Circuit City","name":"Jason Alexander","id":100}""")

      val arr = ArrayTest(100, "debasish", Array("abc", "def", "ghi"))
      toJSON_n(arr) must equal("""{"addresses":["abc","def","ghi"],"name":"debasish","id":100}""")
    }
  }

  describe("from json of an object") {
    it("should de-serialize from json") {
      // simple object
      val s = Shop("kmart", "fridge", 100)
      val js1 = Js("""{"price":100,"item":"fridge","store":"kmart"}""")
      fromJsObject[Shop](js1) must equal(s)

      // object with an Option data type
      // Case 1: Some
      val oa1 = AddressWithOptionalCity("Tamarac Street", Some("Denver"), "80231")
      val js2 = Js(toJSON_n(oa1))
      fromJsObject[AddressWithOptionalCity](js2) must equal(oa1)

      // Case 2: None
      val oa2 = AddressWithOptionalCity("Tamarac Street", None, "80231")
      val js3 = Js(toJSON_n(oa2))
      fromJsObject[AddressWithOptionalCity](js3) must equal(oa2)

      // object with an embedded Map with objects as values
      val c = Contact("debasish ghosh", Map("primary" -> Address("Tamarac Street", "Denver", "80231"), 
                                            "secondary" -> Address("Garden Street", "Cupertino", "95054")))
      val js4 = Js(toJSON_n(c))
      fromJsObject[Contact](js4) must equal(c)

      // object with an embedded List of objects
      val p = Person("ghosh", "debasish", 
                     List(Address("Tamarac Street", "Denver", "80231"), Address("Garden Street", "Cupertino", "95054")))
      val js5 = Js(toJSON_n(p))
      fromJsObject[Person](js5) must equal(p)

      // a complex object
      val addresses = List(Address("10 Market Street", "San Francisco, CA", "94111"),
                           Address("3300 Tamarac Drive", "Denver, CO", "98301"))
      val e = new Employee(100,
                           "Jason Alexander",
                           "Circuit City",
                           addresses,
                           Salary(4500, 245))
      val js6 = Js(toJSON_n(e))
      val em = fromJsObject[Employee](js6)
      em.id must equal(e.id)
      em.name must equal(e.name) 
      em.prevEmployer must equal(e.prevEmployer)
      em.addresses must equal(e.addresses)
      em.sal must equal(e.sal)
    }
  }

  describe("from json of an object 2") {
    it("should de-serialize from json") {
      // object containing Date
      val s = SecurityTrade("trd-001", Calendar.getInstance.getTime, Calendar.getInstance.getTime, BigDecimal(100.45))
      val js1 = Js(toJSON_n(s))
      fromJsObject[SecurityTrade](js1) must equal(s)

      // object containing various types of numeric data 
      val b = Bar("debasish", 12, 100l, 45.65f, true)
      val js2 = Js(toJSON_n(b))
      fromJsObject[Bar](js2) must equal(b)

      val f = Foo("debasish", true)
      val js3 = Js(toJSON_n(f))
      fromJsObject[Foo](js3) must equal(f)

      val t = MyTuple2Message("i001", ("debasish", Shop("KMart", "dress-material", 100)))
      val js4 = Js(toJSON_n(t))
      fromJsObject[MyTuple2Message](js4) must equal(t)

      val c = new ContactWithOptionalAddr("Debasish Ghosh",
        Some(Map("primary" -> new Address("10 Market Street", "San Francisco, CA", "94111"),
            "secondary" -> new Address("3300 Tamarac Drive", "Denver, CO", "98301"))))
      val js5 = Js(toJSON_n(c))
      fromJsObject[ContactWithOptionalAddr](js5) must equal(c)

      // val arr = ArrayTest(100, "debasish", Array("abc", "def", "ghi"))
      // val js4 = Js(toJSON_n(arr))
      // println(fromJsObject[ArrayTest](js4))   // should equal(f)
    }
  }
}
