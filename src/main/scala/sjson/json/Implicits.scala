package sjson.json

object Implicits {
  implicit def ignoreProps = List[String]("class")
  implicit val quoteChar = '"'  

}
