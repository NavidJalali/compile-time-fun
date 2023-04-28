import breakfast.*

extension [A](a: A)
  def asJson(using encoder: Encoder[A]): Json =
    encoder.encode(a)

extension (json: Json)
  def as[A](using decoder: Decoder[A]): Either[String, A] =
    decoder.decode(json)

extension (str: String)
  def decode[A](using decoder: Decoder[A]): Either[String, A] =
    Json.parse(str).flatMap(decoder.decode)

object CreateJsonType extends App {
  val myJson = Json.fromFields(
    "name" -> Json.JString("John Doe"),
    "age"  -> Json.JNumber(42),
    "address" -> Json.fromFields(
      "street"  -> Json.JString("10 Downing Street"),
      "city"    -> Json.JString("London"),
      "country" -> Json.JString("UK")
    ),
    "phoneNumbers" -> Json.fromItems(
      Json.JString("+44 1234567"),
      Json.JString("+44 2345678")
    )
  )

  println(
    myJson.noSpaces
  )
}

object CreateEncoder extends App {

  // Product

  final case class Person(name: String, age: Int) derives Encoder

  val personEncoder = summon[Encoder[Person]]
  val myPerson      = Person("John Doe", 42)

  println(
    myPerson.asJson.noSpaces
  )

   // Sum
  sealed trait Shape derives Encoder

  object Shape {
    final case class Circle(radius: Double)                   extends Shape derives Encoder
    final case class Rectangle(width: Double, height: Double) extends Shape derives Encoder
    final case class Square(side: Double)                     extends Shape derives Encoder
    final case class Triangle(base: Double, height: Double)   extends Shape derives Encoder
  }

  val shapeEncoder   = summon[Encoder[Shape]]
  val myShape: Shape = Shape.Circle(1.0)

  println(
    myShape.asJson.noSpaces
  )
}

object CreateDecoder extends App {

  case class Person(name: String, age: Int) derives Decoder

  val personDecoder = summon[Decoder[Person]]
  val myPerson      = """{"name":"John Doe","age":42}"""

  println(
    myPerson.decode[Person]
  )

  sealed trait Shape derives Decoder

  object Shape {
    final case class Circle(radius: Double)                   extends Shape derives Decoder
    final case class Rectangle(width: Double, height: Double) extends Shape derives Decoder
    final case class Square(side: Double)                     extends Shape derives Decoder
    final case class Triangle(base: Double, height: Double)   extends Shape derives Decoder
  }

  val shapeDecoder = summon[Decoder[Shape]]
  val myShape      = """{"Circle":{"radius":1.0}}"""

  println(
    myShape.decode[Shape]
  )
}
