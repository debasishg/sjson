package sjson.json

object Driver {
  def main(args: Array[String]) {
    (new JsonSpec).execute()
    (new SerializerSpec).execute()
  }
}
