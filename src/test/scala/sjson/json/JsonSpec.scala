package sjson.json

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scala.reflect.BeanInfo

import TestBeans._


class JsonSpec extends Spec with ShouldMatchers {
  import dispatch.json._
  import Js._
  
  implicit def ignoreProps = List[String]("class")
  
  val addr = new Address("garer math", "kolkata", "700075")
  val jsAddr = Js(JsBean.toJSON(addr))
  
  val expected_map = Map(
    JsString("city") -> JsString("kolkata"), 
    JsString("street") -> JsString("garer math"), 
    JsString("zip") -> JsString("700075")
  )
  
  val addresses = List[Address](
    new Address("10 Market Street", "San Francisco, CA", "94111"),
    new Address("3300 Tamarac Drive", "Denver, CO", "98301")
  )
  val person = new Person("Ghosh", "Debasish", addresses)
  val jsPerson = Js(JsBean.toJSON(person))
  
  val expected_person_map = Map(
    JsString("lastName") -> JsString("Ghosh"),
    JsString("firstName") -> JsString("Debasish"),
    JsString("addresses") -> 
      JsArray(
        List(
          JsObject(Map(
            JsString("street") -> JsString("10 Market Street"),
            JsString("city") -> JsString("San Francisco, CA"),
            JsString("zip") -> JsString("94111")
          )),
          JsObject(Map(
            JsString("street") -> JsString("3300 Tamarac Drive"),
            JsString("city") -> JsString("Denver, CO"),
            JsString("zip") -> JsString("98301")
          ))
        )
      )
  )
  
  val b = new Book(100, "A Beautiful Mind", "012-456372")
  val jsBook = Js(JsBean.toJSON(b))
  val expected_book_map = Map(
    JsString("id") -> JsNumber(100),
    JsString("title") -> JsString("A Beautiful Mind"),
    JsString("ISBN") -> JsString("012-456372")
  )
  
  val j = new Journal(100, "IEEE Computer", "Alex Payne", "012-456372")
  val jsJournal = Js(JsBean.toJSON(j))
  val expected_journal_map = Map(
    JsString("id") -> JsNumber(100),
    JsString("title") -> JsString("IEEE Computer"),
    JsString("author") -> JsString("Alex Payne")
  )
  
  val j_1 = new Journal_1(100, "IEEE Computer", "Alex Payne", null)
  val jsJournal_1 = Js(JsBean.toJSON(j_1))
  val expected_journal_1_map = Map(
    JsString("id") -> JsNumber(100),
    JsString("title") -> JsString("IEEE Computer"),
    JsString("author") -> JsString("Alex Payne")
  )
  
  val j_2 = new Journal_2(100, "IEEE Computer", "Alex Payne", "012-456372")
  val jsJournal_2 = Js(JsBean.toJSON(j_2))
  val expected_journal_2_map = Map(
    JsString("id") -> JsNumber(100),
    JsString("title") -> JsString("IEEE Computer"),
    JsString("author") -> JsString("Alex Payne"),
    JsString("ISSN") -> JsString("012-456372")
  )
  
  val b_1 = new Book_1("Programming Scala", new Author("Odersky", "Martin"))
  val jsBook_1 = Js(JsBean.toJSON(b_1))
  val expected_book_1_map = Map(
    JsString("title") -> JsString("Programming Scala"),
    JsString("author") -> JsObject(Map(
      JsString("lastName") -> JsString("Odersky"),
      JsString("firstName") -> JsString("Martin")
    ))
  )
  
  describe("Json from simple Bean") {
    it("should equal expected_map") {
      jsAddr.self should equal (expected_map)
    }
    it("should equal expected_book_map") {
      jsBook.self should equal (expected_book_map)
    }
    it("should match annotated property value for isbn") {
      jsBook.self.asInstanceOf[Map[JsString, JsValue]].get(JsString("ISBN")).get.self should equal ("012-456372")
    }
    it("should ignore annotated property and equal expected_journal_map") {
      jsJournal.self should equal (expected_journal_map)
    }
    it("should ignore issn since it is null equal expected_journal_1_map") {
      jsJournal_1.self should equal (expected_journal_1_map)
    }
    it("should not ignore issn since it is not null, but emit the changed property name and equal expected_journal_2_map") {
      jsJournal_2.self should equal (expected_journal_2_map)
    }
  }
  
  describe("Json from bean with aggregate members") {
    it("should equal expected_person_map") {
      jsPerson.self should equal (expected_person_map)
    }
    it("should equal expected_book_1_map") {
      jsBook_1.self should equal (expected_book_1_map)
    }
  }
  
  val addressStr = """{"street": "garer math", "city": "kolkata", "zip": "700075"}"""
  val addrBean = JsBean.fromJSON(Js(addressStr), Some(classOf[Address]))
  
  describe("Simple bean from Json string") {
    it("should equal addr") {
      val a = addrBean.asInstanceOf[Address] 
      a.street should equal (addr.street)
      a.city should equal (addr.city)
      a.zip should equal (addr.zip)
    }
  }
  
  val addressOptCityStr = """{"street": "garer math", "city": "kolkata", "zip": "700075"}"""
  val addrOptCityBean = JsBean.fromJSON(Js(addressOptCityStr), Some(classOf[AddressWithOptionalCity]))
  val addrOptCity = new AddressWithOptionalCity("garer math", Some("kolkata"), "700075")
  
  describe("Simple bean with Option from Json string") {
    it("should equal addrOptCity") {
      val a = addrOptCityBean.asInstanceOf[AddressWithOptionalCity] 
      a.street should equal (addrOptCity.street)
      a.city.get should equal (addrOptCity.city.get)
      a.zip should equal (addrOptCity.zip)
    }
  }
  
  val addrStr = """{"street": "garer math", "city": "kolkata", "zip": "700075", "ignore1": "debasish", "ignore2": 324}"""
  val addrBean1 = JsBean.fromJSON(Js(addrStr), Some(classOf[Address]))
  
  describe("Bean from Json string with additional key/values") {
    it("should equal addr and ignore additional properties from json") {
      val a = addrBean1.asInstanceOf[Address] 
      a.street should equal (addr.street)
      a.city should equal (addr.city)
      a.zip should equal (addr.zip)
    }
  }
  
  val personStr = """{
    "lastName": "Ghosh",
    "firstName": "Debasish",
    "addresses": [
      {"street": "10 Market Street", "city": "San Francisco, CA", "zip": "94111"},
      {"street": "3300 Tamarac Drive", "city": "Denver, CO", "zip": "98301"}
    ]
  }"""
  val personBean = JsBean.fromJSON(Js(personStr), Some(classOf[Person]))
  
  describe("Bean with aggregate member from Json string") {
    it("should equal person") {
      val p = personBean.asInstanceOf[Person] 
      p.lastName should equal (person.lastName)
      p.firstName should equal (person.firstName)
      p.addresses.size should equal (person.addresses.size)
      p.addresses.map(_.street).mkString(",") should equal (person.addresses.map(_.street).mkString(","))
      p.addresses.map(_.city).mkString(",") should equal (person.addresses.map(_.city).mkString(","))
      p.addresses.map(_.zip).mkString(",") should equal (person.addresses.map(_.zip).mkString(","))
    }
  }
  
  val bookStr = """{
    "title": "Effective C++",
    "author": {
      "lastName": "Myers",
      "firstName": "Scott"
    }
  }"""
  val bookBean = JsBean.fromJSON(Js(bookStr), Some(classOf[Book_1]))
  val expected_book = new Book_1("Effective C++", new Author("Myers", "Scott"))
  
  describe("Bean with class object data member from Json string") {
    it("should equal expected_book") {
      val b = bookBean.asInstanceOf[Book_1] 
      b.title should equal (expected_book.title)
      b.author.firstName should equal (expected_book.author.firstName)
      b.author.lastName should equal (expected_book.author.lastName)
    }
  }
  
  val itemStr_1 = """{
    "_id": "4d3a0a5104c072e8bde5d92d3d2a66ee",
    "_rev": "3749830312",
    "item": "apple",
    "prices": {"Fresh Mart":1.59,"Price Max":5.99,"Apples Express":0.79}
  }"""

  val itemBean_1 = JsBean.fromJSON(Js(itemStr_1), Some(classOf[Item_1]))
  val expected_item_1 = new Item_1("apple", Map("Fresh Mart" -> 1.59, "Price Max" -> 5.99, "Apples Express" -> 0.79))
  
  describe("Bean with Map data member from Json string") {
    it("should equal expected_item_1") {
      val i = itemBean_1.asInstanceOf[Item_1] 
      i.item should equal (expected_item_1.item)
      i.prices.get("Fresh Mart") should equal (expected_item_1.prices.get("Fresh Mart"))
      i.prices should equal (expected_item_1.prices)
    }
  }
  
  val itemStr_2 = """{
    "_id": "4d3a0a5104c072e8bde5d92d3d2a66ee",
    "_rev": "3749830312",
    "item": "apple",
    "prices": [1.59,5.99,0.79]
  }"""

  val itemBean_2 = JsBean.fromJSON(Js(itemStr_2), Some(classOf[Item_2]))
  val expected_item_2 = new Item_2("apple", List(1.59, 5.99, 0.79))
  
  describe("Bean with List data member from Json string") {
    it("should equal expected_item_2") {
      val i = itemBean_2.asInstanceOf[Item_2] 
      i.item should equal (expected_item_2.item)
      i.prices should equal (expected_item_2.prices)
    }
  }
  
  val contactStr = """{
    "name": "Debasish Ghosh",
    "addresses": {
      "primary": {"street": "10 Market Street", "city": "San Francisco, CA", "zip": "94111"},
      "secondary": {"street": "3300 Tamarac Drive", "city": "Denver, CO", "zip": "98301"}
    }
  }"""
  
  val contactBean = JsBean.fromJSON(Js(contactStr), Some(classOf[Contact]))
  val expected_contact = 
    new Contact("Debasish Ghosh",
      Map("primary" -> new Address("10 Market Street", "San Francisco, CA", "94111"),
          "secondary" -> new Address("3300 Tamarac Drive", "Denver, CO", "98301")))
  
  describe("Bean with Map data member that has an object as value from Json string") {
    it("should equal expected_contact") {
      val c = contactBean.asInstanceOf[Contact] 
      c.name should equal (expected_contact.name)
      c.addresses.map(_._2).forall(x => x.isInstanceOf[Address] == true)
      c.addresses.map(a => a._1 + ":" + a._2).mkString(",") should equal (expected_contact.addresses.map(a => a._1 + ":" + a._2).mkString(","))
    }
  }
  
  val contactBeanOptionalAddr = JsBean.fromJSON(Js(contactStr), Some(classOf[ContactWithOptionalAddr]))
  val expected_contact_with_addr = 
    new ContactWithOptionalAddr("Debasish Ghosh",
      Some(Map("primary" -> new Address("10 Market Street", "San Francisco, CA", "94111"),
          "secondary" -> new Address("3300 Tamarac Drive", "Denver, CO", "98301"))))
  
  describe("Bean with Optional Map data member that has an object as value from Json string") {
    it("should equal expected_contact_with_addr") {
      val c = contactBeanOptionalAddr.asInstanceOf[ContactWithOptionalAddr] 
      c.name should equal (expected_contact_with_addr.name)
      c.addresses.get.map(_._2).forall(x => x.isInstanceOf[Address] == true)
      c.addresses.get.map(a => a._1 + ":" + a._2).mkString(",") should equal (expected_contact_with_addr.addresses.get.map(a => a._1 + ":" + a._2).mkString(","))
    }
  }
  
  import TestBeans._
  describe("Generating bean with @JSONProperty annotation for a primitive data member") {
    val ins = Instrument(123, "IBM Securities", "Equity")
    val expected_js_str = """{"id":123,"name":"IBM Securities","TYPE":"Equity"}"""
    var j: JsValue = null
    
    it("should equal expected_js_str") {
      val js = JsBean.toJSON(ins)
      js should equal(expected_js_str)
      j = Js(js)
      val s = (Symbol("TYPE") ? str)
      val s(_s) = j
      _s should equal("Equity")
    }
    it("should create the bean and be equal to ins") {
      val in = JsBean.fromJSON(j, Some(classOf[Instrument]))
      in.typ should equal(ins.typ)
    }
  }
  
  describe("Generating bean with @JSONProperty annotation for an object member") {
    val ins = Instrument(123, "IBM Securities", "Equity")
    val trd = Trade("ref-123", ins, 23400)
    val expected_js_str = """{"amount":23400,"Instrument":{"id":123,"name":"IBM Securities","TYPE":"Equity"},"ref":"ref-123"}"""
    var j: JsValue = null
    
    it("should equal expected_js_str") {
      val js = JsBean.toJSON(trd)
      js should equal(expected_js_str)
      j = Js(js)
      val s = (Symbol("Instrument") ? obj)
      val s(_s) = j
      _s should equal(Js("""{"id":123,"name":"IBM Securities","TYPE":"Equity"}"""))
    }
    it("should create the bean and be equal to ins") {
      val tr = JsBean.fromJSON(j, Some(classOf[Trade]))
      tr.ref should equal(trd.ref)
      tr.amount should equal(trd.amount)
      tr.ins.id should equal(trd.ins.id)
      tr.ins.name should equal(trd.ins.name)
    }
  }
  
  describe("Generating JSON from non-beans") {
    it("should generate correct JSON") {
      JsBean.toJSON("Debasish Ghosh") should equal("\"Debasish Ghosh\"")
      JsBean.toJSON(new java.math.BigDecimal("120.98")) should equal("120.98")
      JsBean.toJSON(List(1, 2, 3)) should equal("[1,2,3]")
      JsBean.toJSON(Map(1 -> "dg", 2 -> "mc")) should equal("""{"1":"dg","2":"mc"}""")
    }
    it("should pass for empty Map") {
      JsBean.toJSON(Map()) should equal("{}")
    }
    it("should pass for empty List in value") {
      JsBean.toJSON(Map("nil" -> Nil)) should equal("""{"nil":[]}""")
    }
    it("should pass for empty Map in value") {
      JsBean.toJSON(Map("empty" -> Map())) should equal("""{"empty":{}}""")
    }
      
  }
  
  describe("Generating JSON from a complex bean") {
    var js: String = null
    var e: Employee = null
    
    it("should match expected_e_js") {
      val expected_e_js = """{"Addresses":[{"city":"San Francisco, CA","street":"10 Market Street","zip":"94111"},{"city":"Denver, CO","street":"3300 Tamarac Drive","zip":"98301"}],"id":100,"name":"Jason Alexander","Previous Employer":"Circuit City","Salary":{"allowance":245,"basic":4500}}"""
      e = 
        new Employee(
          100,
          "Jason Alexander",
          "Circuit City",
          addresses,
          Salary(4500, 245)
        )
      js = JsBean.toJSON(e)
      js should equal(expected_e_js)
    }
    it("generating bean from js should give back e") { 
      val e_b = JsBean.fromJSON(Js(js), Some(classOf[Employee]))
      e_b.name should equal(e.name)
      e_b.addresses.size should equal(e.addresses.size)
    }
    it("should match expected_e_js_1") {
      val expected_e_js_1 = """{"Addresses":[{"city":"San Francisco, CA","street":"10 Market Street","zip":"94111"},{"city":"Denver, CO","street":"3300 Tamarac Drive","zip":"98301"}],"id":100,"name":"Jason Alexander","Salary":{"allowance":245,"basic":4500}}"""
      e = 
        new Employee(
          100,
          "Jason Alexander",
          null,
          addresses,
          Salary(4500, 245)
        )
      js = JsBean.toJSON(e)
      js should equal(expected_e_js_1)
    }
  }
  
  describe("Testing JSON suite of Twitter scala-json") {
    it("should match xml") {
      val JsString(x) = JsValue.fromString("\"<xml>sucks</xml>\"")
      x should equal("<xml>sucks</xml>")
    }
    
    it("should parse strings in double slashes like the ones found in URLs") {
      val JsArray(x) = JsValue.fromString("""["hey! http:\/\/www.lollerskates.com"]""")
      x should equal(List(JsString("hey! http://www.lollerskates.com")))
    }
    
    it("should parse strings with quoted newline") {
      val JsArray(x) = JsValue.fromString("""["hi\njerk"]""")
      x should equal(List(JsString("hi\njerk")))
    }
    
    it("should parse strings with quoted quote") {
      val JsArray(x) = JsValue.fromString("""["x\"x"]""")
      x should equal(List(JsString("x\"x")))                                    
    }
  }
}

