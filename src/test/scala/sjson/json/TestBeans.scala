package sjson
package json

object TestBeans {
  case class Shop(store: String, item: String, price: Number)
  
  case class ShopWithNoDefaultConstructor(store: String, item: String, price: Number)
  
  case class Contact(name: String, 
                     addresses: Map[String, Address])

  case class Address(street: String, city: String, zip: String)

  class InternationalAddress(st: String, ct: String, zp: String, cnt: String)
    extends Address(st, ct, zp) {

    override val street = st
    override val city = ct
    override val zip = zp
    val country = cnt
  }
  
  case class AddressWithOptionalCity(street: String, city: Option[String], zip: String)

  case class ContactWithOptionalAddr(name: String, addresses: Option[Map[String, Address]]) 
  
  case class Person(lastName: String, 
               firstName: String,
               addresses: List[Address]) 

  case class Book(id: Number, title: String, isbn: String)

  case class Author(lastName: String, firstName: String)

  case class Book_1(title: String, author: Author)

  case class Journal(id: BigDecimal, 
                     title: String, 
                     author: String, 
                     issn: String) 

  case class Journal_1(id: Int, 
                  title: String, 
                  author: String, 
                  issn: String)

  class Journal_2(i: Int, t: String, au: String, is: String) {
    val id = i
    val title = t
    val author = au
  
    val issn = is
  }

  class Item_1(i: String, ps: Map[String, Number]) {
    val item = i
    val prices = ps
  }

  class Item_2(i: String, ps: List[Number]) {
    val item = i
    val prices = ps
  }
  
  case class Instrument(
    val id: Number, 
    val name: String, 
    val typ: String)
  
  case class Trade(
    val ref: String,
    val ins: Instrument,
    val amount: Number)
  
  case class Salary(val basic: Number, val allowance: Number)
  
  class Employee(
    val id: Number,
    val name: String,
    val prevEmployer: String,
    val addresses: List[Address],
    val sal: Salary)

  case class Foo(str: String, valid: Boolean)

  case class Bar(str: String, int: Int, lng: Long, flt: Float, valid: Boolean)

  import java.util.Date
  case class SecurityTrade(no: String, tdate: Date, sdate: Date, amount: BigDecimal) 

  case class MyMessage(
    val id: String, 
    val value: Tuple2[String, Int])

  case class ArrayTest(
    id: Int,
    name: String,
    var addresses: Array[String])

  case class ObjectArrayTest(
    id: Int,
    name: String,
    var addresses: Array[Address])

  case class Market(
    name: String, 
    shops: Map[Int, Shop], 
    country: String)

  case class MyTuple2Message(
    val id: String, 
    val value: Tuple2[String, Shop])

  case class View(
    val map: String, 
    val reduce: String)

  case class EnumTest(
    start: WeekDay.Value, 
    shape: Shape.Value,
    month: Month.Value,
    work: List[WeekDay.Value],
    months: List[Month.Value])

  import java.util.TimeZone

  case class TimeZoneBean(val value: TimeZone)

  // an object where we have an Int within a Map and would like to
  // keep it an Int after de-serialization
  case class MyJsonObject(val key: String, 
                          val m: Map[String, Int], 
                          val l: List[Int], 
                          val i: Int)

  case class SampleConfigOption(user: Option[String], names: List[String])

  case class Manager(name: String,
    age: Option[Long])

  case class DesignDocument(var _id: String, 
    _rev: String, 
    views: Map[String, View],
    validate_doc_update: Option[String] = None,
    shows: Option[Map[String, String]] = None,
    lists: Option[Map[String, String]] = None)

  case class Family(
    father: Option[Personz] = None, 
    mother: Option[Personz] = None, 
    children: List[Personz] = List())

  case class Personz(name: String = "", age: Int = -1)

  case class Security[T <: SecurityType](name: String, securityType: T)

  sealed trait SecurityType
  case object STOCK extends SecurityType
  case object FI extends SecurityType
  case object MUTUAL_FUND extends SecurityType
  case object GOVT_BOND extends SecurityType

  case class BondTrade(
    instrument: Security[FI.type],
    taxFees: Option[Map[String, BigDecimal]] = None,
    account: String)

  case class TradedIn(
    securityTypes: Map[String, List[SecurityType]])

  case class MapOfListOfString(securityTypes: Map[String, List[String]])

  case class MapOfListOfShop(
    shops: Map[String, List[Shop]])

  case class OptionalMapOfListOfString(
    securityTypes: Option[Map[String, List[String]]])

  case class MapOfOptionalListOfString(
    securityTypes: Map[String, Option[List[String]]])

  case class OptionalMapOfListOfShop(
    shops: Option[Map[String, List[Shop]]])

  case class MapOfOptionalString(
    securityTypes: Map[String, Option[String]])

  case class MapOfOptionalShop(
    shopTypes: Map[String, Option[Shop]])

  case class ListOfMap(
    shops: List[Map[String, Shop]])

  case class ListOfMapOfString(
    shops: List[Map[String, String]])

  case class ListOfMapOfOptionalString(
    shops: List[Map[String, Option[String]]])

  case class User(email: String, pass: String)
}

object Shape extends Enumeration {
  type Shape = Value
  val Rectangle, Square, Circle, Pentagon, Rhombus = Value
}

object WeekDay extends Enumeration {
  type WeekDay = Value
  val Mon = Value("Monday")
  val Tue = Value("Tuesday")
  val Wed = Value("Wednesday")
  val Thu = Value("Thursday")
  val Fri = Value("Friday")
  val Sat = Value("Saturday")
  val Sun = Value("Sunday")
}

object Month extends Enumeration {
  class Value(val name: String, val dayCount: Int) extends Val(name)
  val January = new Value("January", 31)
  val February = new Value("February", 28)
  val March = new Value("March", 31)
  val April = new Value("April", 30)
  val May = new Value("May", 31)
  val June = new Value("June", 30)
  val July = new Value("July", 31)
  val August = new Value("August", 31)
  val September = new Value("September", 30)
  val October = new Value("October", 31)
  val November = new Value("November", 30)
  val December = new Value("December", 31)
}
