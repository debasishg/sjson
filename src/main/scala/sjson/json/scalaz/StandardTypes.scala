package sjson
package json.scalaz

import scalaz._
import Scalaz._

import dispatch.json._
import JsonSerialization._

trait BasicTypes extends Protocol {
  implicit def optionFormat[T](implicit fmt : Format[T]) : Format[Option[T]] = new Format[Option[T]] {
    def writes(ot: Option[T]) = ot match {
      case Some(t) => tojson(t)
      case None => JsNull
    }
    def reads(json: JsValue) = json match {
      case JsNull => None.success
      case x => some(fromjson[T](x)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]
    }
  }

  <#list 2..6 as i>
  <#assign typeName>
   Tuple${i}[<#list 1..i as j>T${j} <#if i != j>,</#if></#list>]
  </#assign>
  implicit def tuple${i}Format[<#list 1..i as j>T${j}<#if i !=j>,</#if></#list>](implicit 
    <#list 1..i as j>
      fmt${j}: Format[T${j}]<#if i != j>,</#if>
    </#list>
    ): Format[${typeName}] = new Format[${typeName}]{
      def reads (json: JsValue): Validation[NonEmptyList[String], ${typeName}] = {
        val JsArray(<#list 1..i as j>e${j}::</#list> Nil) = json
        (
    <#list 1..i as j>
    fromjson[T${j}](e${j})<#if i != j>|@|</#if>
    </#list>
        ).tupled
      }
      def writes(tuple: ${typeName}) = tuple match {
        case (<#list 1..i as j>t${j}<#if i != j>,</#if></#list>) => JsArray(List(
      <#list 1..i as j>tojson(t${j})(fmt${j})<#if i != j>,</#if></#list>))
        case _ => throw new RuntimeException("Tuple" + ${i} + " expected")
      }
  }
  </#list>
}

trait CollectionTypes extends BasicTypes with Generic {

  implicit def listFormat[T](implicit fmt : Format[T]) : Format[List[T]] = new Format[List[T]] {
    def writes(ts: List[T]) = JsArray(ts.map(t => tojson(t)(fmt)))
    def reads(json: JsValue) = json match {
      case JsArray(ts) => ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T] 
      case _ => "List expected".fail.liftFailNel
    }
  }

  import scala.reflect.Manifest
  implicit def arrayFormat[T](implicit fmt : Format[T], mf: Manifest[T]) : Format[Array[T]] = new Format[Array[T]] {
    def writes(ts: Array[T]) = JsArray((ts.map(t => tojson(t)(fmt))).toList)
    def reads(json: JsValue) = json match {
      case JsArray(ts) => (ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]).map(listToArray(_))
      case _ => "Array expected".fail.liftFailNel
    }
  }
  def listToArray[T: Manifest](ls: List[T]): Array[T] = ls.toArray

  implicit def mapFormat[K, V](implicit fmtk: Format[K], fmtv: Format[V]) : Format[Map[K, V]] = new Format[Map[K, V]] {
    def writes(ts: Map[K, V]) = JsObject(ts.map{case (k, v) => ((tojson(k.toString)).asInstanceOf[JsString], tojson(v)(fmtv))})
    def reads(json: JsValue) = json match {
      case JsObject(m) => 
        val Success(keys) = 
          m.map{ case (k, v) => fromjson[K](k)(fmtk)}.toList.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, K]
        val Success(values) = 
          m.map{ case (k, v) => fromjson[V](v)(fmtv)}.toList.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, V]
        (Map() ++ (keys zip values)).success[NonEmptyList[String]]
      case _ => "Map expected".fail.liftFailNel
    }
  }

  import scala.collection._
  implicit def mutableSetFormat[T](implicit fmt : Format[T]) : Format[mutable.Set[T]] = new Format[mutable.Set[T]] {
    def writes(ts: mutable.Set[T]) = JsArray(ts.toList.map(t => tojson(t)(fmt)))
    def reads(json: JsValue) = json match {
      case JsArray(ts) => 
        (ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]).map(mutable.Set(_: _*))
      case _ => "Set expected".fail.liftFailNel
    }
  }

  implicit def immutableSetFormat[T](implicit fmt : Format[T]) : Format[immutable.Set[T]] = new Format[immutable.Set[T]] {
    def writes(ts: immutable.Set[T]) = JsArray(ts.toList.map(t => tojson(t)(fmt)))
    def reads(json: JsValue) = json match {
      case JsArray(ts) => 
        (ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]).map(immutable.Set(_: _*))
      case _ => "Set expected".fail.liftFailNel
    }
  }
}

trait StandardTypes extends CollectionTypes {

  implicit object BigIntFormat extends Format[BigInt] {
    def writes(o: BigInt) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.toBigInt.success
      case _ => "BigInt expected".fail.liftFailNel
    }
  }

  implicit object BigDecimalFormat extends Format[BigDecimal] {
    def writes(o: BigDecimal) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.success
      case _ => "BigDecimal expected".fail.liftFailNel
    }
  }
}

object BasicTypes {
  /** 2.7/8 compatibility */
  implicit def orderable[A](implicit s: A => Ordered[A]): scala.math.Ordering[A] = new scala.math.Ordering[A] {
    def compare(x: A, y: A) = s(x).compare(y)
  }
}
