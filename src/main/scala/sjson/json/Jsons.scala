package sjson
package json

import java.util.TimeZone
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._
import dispatch.classic.json._
import Util._

trait Jsons {
  /**
   * Convert the <tt>JsValue</tt> to an instance of the class <tt>context</tt>, using the parent for 
   * any annotation hints. Returns an instance of <tt>T</tt>.
   * @todo handling arrays
   * @todo handling default values. If the value of a field is not there in the JsObject, but the
   *       field has a default value in the class, then populate the default value during de-serialization
   */
  def fromJsObject[T: TypeTag](js: JsValue): T = {
    fromJsObject_impl(js, typeOf[T]).asInstanceOf[T]
  }

  import Serialize._
  private[json] def fromJsObject_impl(js: JsValue, tpe: Type): Any = js match {
    case JsObject(m) => {
      val props =
        tpe.members
           .filter(!_.isMethod)
           .map(e => (substringFromLastSep(e.fullName, "."), e.typeSignature))

      val nvs = 
        props.map {prop =>
          val name = prop._1
          val tp = prop._2
          (TermName(name), m.get(JsString(name)).map(in_impl(_, tp)).get)
        }
      instantiate(tpe, nvs.toMap)
    }
    case _ => sys.error("Must be a JsObject")
  }

  private[json] def extract(jsv: JsValue, tpe: Type): Any = {
    val ex = jsv match {
      case JsNumber(n) => n
      case JsString(s) => s 
      case JsArray(l) => {
        // deep serialization
        if (tpe <:< typeOf[Seq[_]]) {
          val intpe = tpe.typeSymbol.asType.typeParams.head.asType.toTypeIn(tpe)
          l.map(in_impl(_, intpe))
        } else {
          // none is serialized as empty list
          List()
        }
      }
      case JsBoolean(b) => b
      case JsNull => null
      case JsObject(mp) => { // either Map or a Tuple2
        // deep serialization
        val targs = tpe.typeSymbol.asType.typeParams.map(_.asType.toTypeIn(tpe))
        val (km, vm) = (targs.head, targs.last)

        if (tpe <:< typeOf[Tuple2[_, _]]) {
          val tup = mp.toList.head
          val deserl_1 = in_impl(tup._1, km)
          val deserl_2 = in_impl(tup._2, vm)

          (deserl_1, deserl_2)
        } else { // Map
          mp.map { case (k, v) =>
            val deserl_k = in_impl(k, km)
            val deserl_v = in_impl(v, vm)
            (deserl_k, deserl_v)
          }
        }
      }
    }
    ex  // .asInstanceOf[T]
  }
    
  def toJSON_n[T: TypeTag](obj: T): String = {
    toJSON_impl(obj, typeOf[T])
  }

  private[json] def toJSON_impl[T](obj: T, tpe: Type): String = obj match {
    case null => "null"
    case s: String => quote(obj.toString)
    case x if tpe <:< definitions.AnyValTpe => obj.toString
    case n: Number => obj.toString
    case d: java.util.Date => quote(d.getTime.toString)
    case d: java.util.TimeZone => quote(d.getID)
    case v: Enumeration#Value => quote(v.toString)
    case s: Seq[_] => {
      val intpe = tpe.typeSymbol.asType.typeParams.head.asType.toTypeIn(tpe)
      s.map(toJSON_impl(_, intpe)).mkString("[", ",", "]")
    }
    case Some(s) => {
      val intpe = tpe.typeSymbol.asType.typeParams.head.asType.toTypeIn(tpe)
      toJSON_impl(s, intpe)
    }
    case None => "[]"
    case s: Array[_] => {
      val intpe = tpe.typeSymbol.asType.typeParams.head.asType.toTypeIn(tpe)
      s.map(toJSON_impl(_, intpe)).mkString("[", ",", "]")
    }
    case m: Map[_, _] => {
      val intpe = tpe.typeSymbol.asType.typeParams.map(_.asType.toTypeIn(tpe))
      val (k, v) = (intpe.head, intpe.last)
      m.map(e => toJSON_impl(e._1.toString, typeOf[String]) + ":" + toJSON_impl(e._2, v)).mkString("{", ",", "}")
    }
    case (t: Tuple2[_, _]) => {
      val intpe = tpe.typeSymbol.asType.typeParams.map(_.asType.toTypeIn(tpe))
      val (t1, t2) = (intpe.head, intpe.last)
      "{" + toJSON_impl(t._1, t1) + ":" + toJSON_impl(t._2, t2) + "}"
    }
    // scala case object
    case x if x.getClass.getName.endsWith("$") => {
      quote(obj.getClass.getName)
    }

    case _ => {  // bean
      val props =
        tpe.members
           .filter(!_.isMethod)
           .map(e => (substringFromLastSep(e.fullName, "."), e.typeSignature))
      val kvs =
        props.map {p => 
          val result = getPropertyValue(obj, tpe, p._1)
          toJSON_impl(p._1, typeOf[String]) ++ ":" ++ toJSON_impl(result, p._2)
        }
      kvs.mkString("{", ",", "}")
    }
  }
}

object Jsons extends Jsons 
