package breakfast

sealed trait Json { self =>
  def noSpaces: String =
    self match
      case Json.JNull           => "null"
      case Json.JString(value)  => s"\"$value\""
      case Json.JNumber(value)  => value.toString
      case Json.JBoolean(value) => value.toString
      case Json.JObject(value) =>
        value.map { (key, value) =>
          s""""$key":${value.noSpaces}"""
        }.mkString("{", ",", "}")
      case Json.JArray(value) => value.map(_.noSpaces).mkString("[", ",", "]")
}

object Json {
  case object JNull                                  extends Json
  final case class JString(value: String)            extends Json
  final case class JNumber(value: BigDecimal)        extends Json
  final case class JBoolean(value: Boolean)          extends Json
  final case class JObject(value: Map[String, Json]) extends Json
  final case class JArray(value: Vector[Json])       extends Json

  def fromFields(
    fields: (String, Json)*
  ): Json = JObject(fields.toMap)

  def fromItems(
    items: Json*
  ): Json = JArray(items.toVector)

  def parse(
    input: String
  ): Either[String, Json] =
    Parser.JsonSupport.jsonParser.parse(input)
}
