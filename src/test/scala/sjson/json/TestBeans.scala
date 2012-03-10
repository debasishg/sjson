package sjson
package json

import scala.reflect._
import scala.annotation.target._

object TestBeans {
  @BeanInfo
  case class Shop(store: String, item: String, price: Number) {

    private def this() = this(null, null, null)

    override def toString = "shop = " + store + " for item " + item + " @ " + price
  }

  @BeanInfo
  case class ShopWithNoDefaultConstructor(store: String, item: String, price: Number) {
    override def toString = "shop = " + store + " for item " + item + " @ " + price
  }

  @BeanInfo
  case class Contact(name: String,
                     @(JSONTypeHint @field)(value = classOf[Address])
                     addresses: Map[String, Address]) {

    private def this() = this(null, null)

    override def toString = "name = " + name + " addresses = " + addresses.map(a => a._1 + ":" + a._2.toString).mkString(",")
  }

  @BeanInfo
  case class Address(street: String, city: String, zip: String) {
    private def this() = this(null, null, null)

    override def toString = "address = " + street + "/" + city + "/" + zip
  }

  @BeanInfo
  class InternationalAddress(st: String, ct: String, zp: String, cnt: String)
    extends Address(st, ct, zp) {

    override val street = st
    override val city = ct
    override val zip = zp
    val country = cnt

    private def this() = this(null, null, null, null)

    override def toString = super.toString + "/" + country
  }

  @BeanInfo
  case class AddressWithOptionalCity(street: String, city: Option[String], zip: String) {

    private def this() = this(null, None, null)

    override def toString = "address = " + street + "/" +
      (city match {
        case None => ""
        case Some(c) => c
      }) + "/" + zip
  }


  @BeanInfo
  case class ContactWithOptionalAddr(name: String,
    @(JSONTypeHint @field)(value = classOf[Address])
    @(OptionTypeHint @field)(value = classOf[scala.collection.Map[String,Address]])
    addresses: Option[Map[String, Address]]) {

    private def this() = this(null, None)

    override def toString = "name = " + name + " " +
      (addresses match {
        case None => ""
        case Some(ad) => " addresses = " + ad.map(a => a._1 + ":" + a._2.toString).mkString(",")
      })
  }


  @BeanInfo
  case class Person(lastName: String,
               firstName: String,
               @(JSONTypeHint @field)(value = classOf[Address])
               addresses: List[Address]) {

    def this() = this(null, null, Nil)

    override def toString = "person = " + lastName + "/" + firstName + "/" + addresses
  }

  @BeanInfo
  case class Book(id: Number,
             title: String, @(JSONProperty @getter)(value = "ISBN") isbn: String) {

    def this() = this(0, null, null)
    override def toString = "id = " + id + " title = " + title + " isbn = " + isbn
  }

  @BeanInfo
  case class Author(lastName: String, firstName: String) {

    private def this() = this(null, null)
  }

  @BeanInfo
  case class Book_1(title: String, author: Author) {

    private def this() = this(null, null)
  }

  @BeanInfo
  case class Journal(id: BigDecimal,
                     title: String,
                     author: String,
                     @(JSONProperty @getter)(ignore = true) issn: String) {

    private def this() = this(0, null, null, null)
    override def toString =
      "Journal: " + id + "/" + title + "/" + author +
        (issn match {
            case null => ""
            case _ => "/" + issn
          })
  }

  @BeanInfo
  case class Journal_1(id: Int,
                  title: String,
                  author: String,
                  @(JSONProperty @getter)(ignoreIfNull = true) issn: String) {
  }

  @BeanInfo
  class Journal_2(i: Int, t: String, au: String, is: String) {
    val id = i
    val title = t
    val author = au

    @(JSONProperty @getter)(value = "ISSN", ignoreIfNull = true)
    val issn = is
  }

  @BeanInfo
  class Item_1(i: String, ps: Map[String, Number]) {
    val item = i
    val prices = ps

    def this() = this(null, null)
  }

  @BeanInfo
  class Item_2(i: String, ps: List[Number]) {
    val item = i
    val prices = ps

    def this() = this(null, null)
  }

  @BeanInfo
  case class Instrument(
    val id: Number,
    val name: String,
    @(JSONProperty @getter)(value = "TYPE", ignoreIfNull = false, ignore = false)
    val typ: String) {

    private def this() = this(null, null, null)
    override def toString = "id: " + id + " name: " + name + " type: " + typ
  }

  @BeanInfo
  case class Trade(
    val ref: String,
    @(JSONProperty @getter)(value = "Instrument", ignoreIfNull = false, ignore = false)
    val ins: Instrument,
    val amount: Number) {

    private def this() = this(null, null, null)
    override def toString = "ref: " + ref + " ins: " + ins + " amount: " + amount
  }

  @BeanInfo
  case class Salary(val basic: Number, val allowance: Number) {
    private def this() = this(null, null)
  }

  @BeanInfo
  class Employee(
    val id: Number,
    val name: String,

    @(JSONProperty @getter)(value = "Previous Employer", ignoreIfNull = true, ignore = false)
    val prevEmployer: String,

    @(JSONProperty @getter)(value = "Addresses")
    @(JSONTypeHint @field)(value = classOf[Address])
    val addresses: List[Address],

    @(JSONProperty @getter)(value = "Salary")
    val sal: Salary
  ) {
    private def this() = this(null, null, null, Nil, null)
  }

  @BeanInfo
  case class Foo(str: String, valid: Boolean) {
    private def this() = this(null, false)
  }

  @BeanInfo
  case class Bar(str: String, int: Int, lng: Long, flt: Float, valid: Boolean) {
    private def this() = this(null, 0, 0l, 0f, false)
  }

  import java.util.Date
  @BeanInfo
  case class SecurityTrade(no: String, tdate: Date, sdate: Date, amount: BigDecimal) {
    private def this() = this(null, null, null, null)
  }

  @BeanInfo
  case class MyMessage(
    val id: String,
    val value: Tuple2[String, Int]) {
    private def this() = this(null, null)
  }

  @BeanInfo
  case class ArrayTest(
    id: Int,
    name: String,
    @(JSONTypeHint @field)(value = classOf[String])
    var addresses: Array[String]) {
    def this() = this(0, null, null)
  }

  @BeanInfo
  case class ObjectArrayTest(
    id: Int,
    name: String,
    @(JSONTypeHint @field)(value = classOf[Address])
    var addresses: Array[Address]) {
    def this() = this(0, null, null)
  }

  @BeanInfo
  case class Market(
    name: String,
    @(JSONTypeHint @field)(value = classOf[Shop])
    shops: Map[Int, Shop],
    country: String) {
    private def this() = this(null, null, null)
  }

  @BeanInfo
  case class MyTuple2Message(
    val id: String,
    @(JSONTypeHint @field)(value = classOf[Shop])
    val value: Tuple2[String, Shop]) {
    private def this() = this(null, null)
  }

  @BeanInfo
  case class View(
    @(JSONProperty @getter)(ignoreIfNull = true)
    val map: String,

    @(JSONProperty @getter)(ignoreIfNull = true)
    val reduce: String) {

    private def this() = this(null, null)

    override def toString =
      "map: " + map + " reduce: " + reduce
  }

  @BeanInfo
  case class EnumTest(
    @(EnumTypeHint @field)(value = "sjson.json.WeekDay") start: WeekDay.Value,
    @(EnumTypeHint @field)(value = "sjson.json.Shape") shape: Shape.Value,
    month: Month.Value,
    @(JSONTypeHint @field)(value = classOf[sjson.json.WeekDay.WeekDay])
    @(EnumTypeHint @field)(value = "sjson.json.WeekDay")
    work: List[WeekDay.Value],
    @(JSONTypeHint @field)(value = classOf[sjson.json.Month.Value])
    @(EnumTypeHint @field)(value = "sjson.json.Month")
    months: List[Month.Value]) {
    private def this() = this(null, null, null, null, null)
  }

  import java.util.TimeZone

  @BeanInfo
  case class TimeZoneBean(
    val value: TimeZone
  ) {
    private def this() = this(null)
  }

  // an object where we have an Int within a Map and would like to
  // keep it an Int after de-serialization
  @BeanInfo
  case class MyJsonObject(val key: String,
                          @(JSONTypeHint @field)(value = classOf[Int])
                          val m: Map[String, Int],
                          @(JSONTypeHint @field)(value = classOf[Int])
                          val l: List[Int],
                          val i: Int) {
    private def this() = this(null, null, null, -1)
    override def toString = {
      val inte: Int = m.getOrElse(key, -1)
      "key = " + key + " mapping to " + inte + " original int = " + i
    }
  }

  @BeanInfo case class SampleConfigOption(user: Option[String], names: List[String]) {
    def this() = this (Some("default"), List[String]())
  }

  @BeanInfo
  case class Manager(name: String,
    @(OptionTypeHint@field)(value = classOf[Long]) age: Option[Long]) {
     private def this() = this ("", None)
  }

  @BeanInfo
  case class DesignDocument(var _id: String,
    @(JSONProperty @getter)(ignoreIfNull = true, ignore = false) _rev: String,
    @(JSONTypeHint @field)(value = classOf[View]) views: Map[String, View],
    @(JSONProperty @getter)(ignoreIfNull = true, ignore = false) validate_doc_update: Option[String] = None,
    @(JSONProperty @getter)(ignoreIfNull = true, ignore = false) shows: Option[Map[String, String]] = None,
    @(JSONProperty @getter)(ignoreIfNull = true, ignore = false) lists: Option[Map[String, String]] = None) {
    private def this() = this(null, null, Map[String, View]())

    override def toString = {
    "_id = " + _id + " _rev = " + _rev + " " + " validate = " + validate_doc_update +
      (views match {
        case null => ""
        case v => {
          v.map(e =>
            (e._1.toString + ":" + e._2.toString)).mkString(",")
        }
      })
    }
  }

  @BeanInfo
  case class Family(
    @(JSONProperty @getter)(ignoreIfNull = true)
    @(OptionTypeHint@field)(value = classOf[Personz]) father: Option[Personz] = None,
    @(JSONProperty @getter)(ignoreIfNull = true)
    @(OptionTypeHint@field)(value = classOf[Personz]) mother: Option[Personz] = None,
    @(JSONProperty @getter)(ignoreIfNull = true)
    @(JSONTypeHint @field)(value = classOf[Personz]) children: List[Personz] = List()) {
    def this() = this(None, None, List())
    }

  @BeanInfo
  case class Personz(@(JSONProperty @getter)(ignoreIfNull = true) name: String = "", age: Int = -1) {
    def this() = this("", -1)
  }

  @BeanInfo
  case class Security[T <: SecurityType](name: String, securityType: T) {
    def this() = this("", null.asInstanceOf[T])
  }

  sealed trait SecurityType
  case object STOCK extends SecurityType
  case object FI extends SecurityType
  case object MUTUAL_FUND extends SecurityType
  case object GOVT_BOND extends SecurityType

  @BeanInfo
  case class BondTrade(
    instrument: Security[FI.type],
    @(JSONProperty @getter)(ignoreIfNull = true, ignore = false) @(OptionTypeHint@field)(value = classOf[scala.collection.Map[_, _]]) taxFees: Option[Map[String, BigDecimal]] = None,
    account: String) {
    def this() = this(null, None, "")
  }

  @BeanInfo
  case class TradedIn(
    @(JSONTypeHint @field)(value = classOf[SecurityType]) securityTypes: Map[String, List[SecurityType]]) {
    def this() = this(Map.empty[String, List[SecurityType]])
  }

  @BeanInfo
  case class MapOfListOfString(securityTypes: Map[String, List[String]]) {
    def this() = this(Map.empty[String, List[String]])
  }

  @BeanInfo
  case class MapOfListOfShop(
    @(JSONTypeHint @field)(value = classOf[Shop]) shops: Map[String, List[Shop]]) {
    def this() = this(Map.empty[String, List[Shop]])
  }

  @BeanInfo
  case class OptionalMapOfListOfString(
    @(OptionTypeHint@field)(value = classOf[scala.collection.Map[_, _]]) securityTypes: Option[Map[String, List[String]]]) {
    def this() = this(None)
  }

  @BeanInfo
  case class MapOfOptionalListOfString(
    @(OptionTypeHint@field)(value = classOf[scala.collection.immutable.List[_]]) securityTypes: Map[String, Option[List[String]]]) {
    def this() = this(Map.empty[String, Option[List[String]]])
  }

  @BeanInfo
  case class OptionalMapOfListOfShop(
    @(OptionTypeHint@field)(value = classOf[scala.collection.Map[_, _]])
    @(JSONTypeHint @field)(value = classOf[Shop]) shops: Option[Map[String, List[Shop]]]) {
    def this() = this(None)
  }

  @BeanInfo
  case class MapOfOptionalString(
    @(OptionTypeHint@field)(value = classOf[String]) securityTypes: Map[String, Option[String]]) {
    def this() = this(Map.empty[String, Option[String]])
  }

  @BeanInfo
  case class MapOfOptionalShop(
    @(OptionTypeHint@field)(value = classOf[Shop]) shopTypes: Map[String, Option[Shop]]) {
    def this() = this(Map.empty[String, Option[Shop]])
  }

  @BeanInfo
  case class ListOfMap(
    @(JSONTypeHint@field)(value = classOf[Shop]) shops: List[Map[String, Shop]]) {
    def this() = this(List.empty[Map[String, Shop]])
  }

  @BeanInfo
  case class ListOfMapOfString(
    @(JSONTypeHint@field)(value = classOf[String]) shops: List[Map[String, String]]) {
    def this() = this(List.empty[Map[String, String]])
  }

  @BeanInfo
  case class ListOfMapOfOptionalString(
    @(OptionTypeHint@field)(value = classOf[String])
    @(JSONTypeHint@field)(value = classOf[String])
    shops: List[Map[String, Option[String]]]) {
    def this() = this(List.empty[Map[String, Option[String]]])
  }
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
