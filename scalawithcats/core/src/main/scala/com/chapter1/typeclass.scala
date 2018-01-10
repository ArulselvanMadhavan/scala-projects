sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

//JsonWriter is the typeclass
trait JsonWriter[A] {
  def write(value: A): Json
}

//Write concrete instances for JsonWriter and tag them with implicit keyword

final case class Person(name: String, email: String)

object JsonWriterInstances {

  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    def write(value: String): Json =
      JsString(value)
  }

  implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
    def write(value: Person): Json =
      JsObject(
        Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        ))
  }
}

//Interface object
object Json {
  def toJson[A: JsonWriter](value: A): Json =
    implicitly[JsonWriter[A]].write(value)
}

//Interface syntax
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w:JsonWriter[A]):Json =
      w.write(value)
  }
}
