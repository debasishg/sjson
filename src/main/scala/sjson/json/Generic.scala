package sjson
package json

import dispatch.json._
trait Generic extends Protocol {

  import JsonSerialization._

  implicit def listFormat[T](implicit fmt : Format[T]) : Format[List[T]];

  def viaSeq[S <: Iterable[T], T] (f: Seq[T] => S) (implicit fmt : Format[T]) : Format[S] = new Format[S] {
    def writes(ts: S) = JsArray(ts.map(t => tojson(t)(fmt)).toList)
    def reads(json: JsValue) = json match {
      case JsArray(ts) => f(ts.map(t => fromjson[T](t)))
      case _ => throw new RuntimeException("Collection expected")
    }
  }

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
      case JsObject(m) =>
        from(fromjson[T](m(JsString(name))))
      case _ => throw new RuntimeException("Object expected")
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

  /**
  case class Summand[T](clazz : Class[_], format : Format[T]);
  implicit def classToSummand[T](clazz : Class[T])(implicit fmt : Format[T]) : Summand[T] = Summand[T](clazz, fmt);
  implicit def formatToSummand[T](format : Format[T])(implicit mf : scala.reflect.Manifest[T]) : Summand[T] = 
    Summand[T](mf.erasure, format);

  def asUnion[S](summands : Summand[_ <: S]*) : Format[S] = 
    if (summands.length >= 256) error("Sums of 256 or more elements currently not supported");
    else
    new Format[S]{
      val mappings = summands.toArray.zipWithIndex;

      def reads(json : JsValue) : S = read(in)(summands(read[Byte](in)).format)
      // def writes(ts: S) = JsArray(ts.map(t => tojson(t)(fmt)).toList)
      // def reads(json: JsValue) = json match {

      def writes(ts: S) = JsArray(ts.map(t => tojson(t)(fmt)).toList)

      def writes(out : Output, s : S): Unit =
        mappings.find(_._1.clazz.isInstance(s)) match {
          case Some( (sum, i) ) => writeSum(out, s, sum, i)
          case None => error("No known sum type for object " + s);
        }
      private def writeSum[T](out : Output, s : S, sum : Summand[T], i : Int) {
        write(out, i.toByte);
        // 2.7/2.8 compatibility: cast added by MH
        write(out, sum.clazz.cast(s).asInstanceOf[T])(sum.format);
      }
  }
**/
}
