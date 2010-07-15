package sjson
package json

import dispatch.json._
trait Generic extends Protocol {

  import JsonSerialization._

  implicit def listFormat[T](implicit fmt : Format[T]) : Format[List[T]];

  <#list 2..9 as i> 
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
      case JsObject(m) => // m is the Map
        apply(
          <#list 1..i as j>
          fromjson[T${j}](m(JsString(f${j})))<#if i != j>,</#if>
          </#list>
        )
      case _ => throw new RuntimeException("object expected")
    }
  }  
  </#list>

  /**
   * one sample for i=3 that will be generated is:
   * def asProduct3[S, T1, T2, T3](f1: String, f2: String, f3: String)(apply : (T1, T2, T3) => S)(unapply : S => Product3[T1, T2, T3])(implicit bin1: Format[T1], bin2: Format[T2], bin3: Format[T3]) = new Format[S]{
   *   def writes(s: S) = {
   *     val product = unapply(s)
   *     JsObject(
   *       List(
   *         (tojson(f1).asInstanceOf[JsString], tojson(product._1)), 
   *         (tojson(f2).asInstanceOf[JsString], tojson(product._2)), 
   *         (tojson(f3).asInstanceOf[JsString], tojson(product._3)) 
   *     ))
   *   }
   *   def reads(js: JsValue) = js match {
   *     case JsObject(m) => // m is the Map
   *       apply(
   *         fromjson[T1](m(JsString(f1))), 
   *         fromjson[T2](m(JsString(f2))), 
   *         fromjson[T3](m(JsString(f3)))
   *       )
   *     case _ => throw new RuntimeException("object expected")
   *   }
   * }  
   **/
}
