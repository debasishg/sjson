package sjson
package json

import scala.reflect.runtime.universe._
import Util._
import Jsons._

/**
 * @author <a href="http://debasishg.blogspot.com">Debasish Ghosh</a>
 */
object Serialize {
  
  import dispatch.classic.json._
  import Js._
  import java.io.{ObjectInputStream, ObjectOutputStream, ByteArrayInputStream, ByteArrayOutputStream}
  import org.apache.commons.io.input.ClassLoaderObjectInputStream

  val classLoader: Option[ClassLoader] = Some(this.getClass.getClassLoader)

  def deepClone[T: TypeTag](obj: T): T = in[T](out(obj.asInstanceOf[AnyRef]))
  
  /**
   * Serialize out a Scala object. It can be serialized back in to the object using
   * <tt>in</tt> method.
   * <p/>
   * <pre>
   * val l = List("ab", "cd")
   * in(out(l)) => ["ab", "cd"]
   * in[List[String]](out(l)) => List("ab", "cd")
   * </pre>
   * <em>Caveat</em>
   * Nulls are serialized as String null ("null"). This may create problems if a String field
   * contains the value "null".
   */
  def out[T: TypeTag](obj: T): Array[Byte] = {
    toJSON_n(obj).getBytes("UTF-8")
  }
  
  def in[T: TypeTag](bytes: Array[Byte]): T = {
    in[T](new String(bytes, "UTF-8"))
  }

  def in[T: TypeTag](json: String): T = {
    in[T](Js(json))
  }

  def in[T: TypeTag](js: JsValue): T = {
    in_impl(js, typeOf[T]).asInstanceOf[T]
  }

  private[json] def in_impl(js: JsValue, tpe: Type): Any = {
    val intpe =
      if (tpe <:< typeOf[Option[_]]) tpe.typeSymbol.asType.typeParams.head.asType.toTypeIn(tpe)
      else tpe

    val kludge =
      js match {
        case JsArray(l) if l.isEmpty => true
        case _ => false
      }

    if (tpe <:< typeOf[Option[_]] && kludge) None
    else {

      val ret =
        // Map and Tuple2 both are serialized as Maps wrapped within a JsObject
        if (intpe <:< typeOf[collection.immutable.Map[_, _]] ||
            intpe <:< typeOf[Tuple2[_, _]]) extract(js, intpe)

        // beans are also serialized as JsObjects, but need to invoke fromJSON for beans
        else if (js.isInstanceOf[JsObject]) {
          fromJsObject_impl(js, intpe)
        }

        // all other cases
        else extract(js, intpe)
      if (intpe =:= tpe) ret else Some(ret)
    }
  }

  /**
   * Serialize in a JSON into a Scala object, specifying a class that can be loaded
   * through an externally specified class loader. 
   * In order to specify the class loader, do the following :
   * <pre>
   * object SJSON extends SJSON {
   *   val classLoader = None
   * }
   * </pre>
   */
  def in[T: TypeTag](json: Array[Byte], clazzName: String): T = {
    val clazz =
      classLoader match {
        case Some(cl) =>
          Class.forName(clazzName, true, cl)
        case None =>
          Class.forName(clazzName)
      }
    in[T](Js(new String(json)))
  }
}
