package sjson
package json

object Implicits {
  implicit def ignoreProps = List[String]("class")
  implicit val quoteChar = '"'  

}
