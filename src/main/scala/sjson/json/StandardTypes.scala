package sjson
package json

import dispatch.json._
import JsonSerialization._

trait BasicTypes extends Protocol {
  implicit def optionFormat[T](implicit fmt : Format[T]) : Format[Option[T]] = new Format[Option[T]] {
    def writes(ot: Option[T]) = ot match {
      case Some(t) => tojson(t)
      case None => JsNull
    }
    def reads(json: JsValue) = json match {
      case JsNull => None
      case x => Some(fromjson[T](x))
    }
  }

  <#list 2..22 as i>
  <#assign typeName>
   Tuple${i}[<#list 1..i as j>T${j} <#if i != j>,</#if></#list>]
  </#assign>
  implicit def tuple${i}Format[<#list 1..i as j>T${j}<#if i !=j>,</#if></#list>](implicit 
    <#list 1..i as j>
      fmt${j}: Format[T${j}]<#if i != j>,</#if>
    </#list>
    ): Format[${typeName}] = new Format[${typeName}]{
      def reads (json: JsValue): ${typeName} = {
        val JsArray(<#list 1..i as j>e${j}::</#list> Nil) = json
        (
    <#list 1..i as j>
    fromjson[T${j}](e${j})<#if i != j>,</#if>
    </#list>
        )
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
      case JsArray(ts) => ts.map(t => fromjson(t)(fmt))
      case _ => throw new RuntimeException("List expected")
    }
  }

  import scala.reflect.Manifest
  implicit def arrayFormat[T](implicit fmt : Format[T], mf: Manifest[T]) : Format[Array[T]] = new Format[Array[T]] {
    def writes(ts: Array[T]) = JsArray((ts.map(t => tojson(t)(fmt))).toList)
    def reads(json: JsValue) = json match {
      case JsArray(ts) => listToArray(ts.map(t => fromjson(t)(fmt)))
      case _ => throw new RuntimeException("Array expected")
    }
  }
  def listToArray[T: Manifest](ls: List[T]): Array[T] = ls.toArray

  implicit def mapFormat[K, V](implicit fmtk: Format[K], fmtv: Format[V]) : Format[Map[K, V]] = new Format[Map[K, V]] {
    def writes(ts: Map[K, V]) = JsObject(ts.map{case (k, v) => ((tojson(k.toString)).asInstanceOf[JsString], tojson(v)(fmtv))})
    def reads(json: JsValue) = json match {
      case JsObject(m) => Map() ++ m.map{case (k, v) => (fromjson[K](k)(fmtk), fromjson[V](v)(fmtv))}
      case _ => throw new RuntimeException("Map expected")
    }
  }

  import scala.collection._
  implicit def mutableSetFormat[T](implicit fmt: Format[T]): Format[mutable.Set[T]] = 
    viaSeq((x: Seq[T]) => mutable.Set(x: _*))

  implicit def immutableSetFormat[T](implicit fmt: Format[T]): Format[immutable.Set[T]] = 
    viaSeq((x: Seq[T]) => immutable.Set(x: _*))

  implicit def immutableSortedSetFormat[S](implicit ord : S => Ordered[S], binS : Format[S]) : Format[immutable.SortedSet[S]] = {
    import BasicTypes.orderable // 2.7/8 compatibility
    viaSeq((x: Seq[S]) => immutable.TreeSet[S](x: _*))
  }
}

trait StandardTypes extends CollectionTypes {
  implicit object BigIntFormat extends Format[BigInt] {
    def writes(o: BigInt) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.toBigInt
      case _ => throw new RuntimeException("BigInt expected")
    }
  }

  implicit object BigDecimalFormat extends Format[BigDecimal] {
    def writes(o: BigDecimal) = JsValue.apply(o)
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n
      case _ => throw new RuntimeException("BigDecimal expected")
    }
  }
}

object BasicTypes {
  /** 2.7/8 compatibility */
  implicit def orderable[A](implicit s: A => Ordered[A]): Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A) = s(x).compare(y)
  }
}
