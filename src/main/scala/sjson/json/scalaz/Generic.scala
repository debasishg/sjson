package sjson
package json.scalaz

import scalaz._
import Scalaz._

import dispatch.json._
trait Generic extends Protocol {

  import JsonSerialization._
  import DefaultProtocol._

  implicit def listFormat[T](implicit fmt : Format[T]) : Format[List[T]];

  /**
   * Use this when you would wrap a value in a case class
   *
   * <pre>
   * case class Name(name: String)
   * implicit val NameFormat: Format[Name] = wrap[Name, String]("name")(_.name, Name)
   *
   * val n = Name("debasish ghosh")
   * fromjson[Name](tojson(n)) should equal(n)
   * </pre>
   */
  def wrap[S, T](name: String)(to : S => T, from : T => S)(implicit fmt : Format[T]) = new Format[S]{
    def writes(s : S) = JsObject(List((tojson(name).asInstanceOf[JsString], tojson(to(s)))))
    def reads(js : JsValue) = js match {
      case m@JsObject(_) =>
        val f = field[T](name, m)
        f match {
          case Success(v) => from(v).success
          case Failure(e) => e.fail
        }
      case _ => "Object expected".fail.liftFailNel
    }
  }

  /**
   * Lazy wrapper around serialization. Useful when you want to serialize mutually recursive structures.
   */
  def lazyFormat[S](fmt : => Format[S]) = new Format[S]{
    lazy val delegate = fmt;

    def reads(js : JsValue) = delegate.reads(js);
    def writes(s : S) = delegate.writes(s);
  }


  <#list 2..8 as i> 
  <#assign typeParams><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>

  def asProduct${i}[S, ${typeParams}](<#list 1..i as j>f${j}: String<#if i != j>,</#if></#list>)(apply : (${typeParams}) => S)(unapply : S => Product${i}[${typeParams}])(implicit <#list 1..i as j>bin${j}: Format[T${j}]<#if i != j>,</#if></#list>) = new Format[S]{
    def writes(s: S) = {
      val product = unapply(s)
      JsObject(
        List(
          <#list 1..i as j>
          (tojson(f${j}).asInstanceOf[JsString], tojson(product._${j}))<#if i != j>,</#if>
          </#list>
        ))
    }
    def reads(js: JsValue) = js match {
      case m@JsObject(_) => // m is the Map
        (
          <#list 1..i as j>
          field[T${j}](f${j}, m)<#if i != j> |@|</#if>
          </#list>
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  </#list>
}
