package breakfast

final case class Parser[+A](run: String => Either[String, (A, String)]) { self =>
  def suchThat(f: A => Boolean, error: A => String): Parser[A] = self.flatMap { a =>
    if (f(a)) Parser.succeed(a) else Parser.fail(error(a))
  }

  def parse(str: String): Either[String, A] = self.run(str).map(_._1)

  def flatMap[B](other: A => Parser[B]): Parser[B] = Parser(
    (
      input =>
        self.run(input) match {
          case Right((a, leftover)) => other(a).run(leftover)
          case Left(err)            => Left(err)
        }
    )
  )

  def zipWith[B, C](that: Parser[B])(f: (A, B) => C): Parser[C] =
    self.flatMap(a => that.map(b => f(a, b)))

  def zip[B](that: Parser[B]): Parser[(A, B)] = zipWith(that)((_, _))

  def zipLeft[B](that: Parser[B]): Parser[A] = zipWith(that)((a, _) => a)

  def zipRight[B](that: Parser[B]): Parser[B] = zipWith(that)((_, b) => b)

  def *>[B](that: Parser[B]): Parser[B] = zipRight(that)

  def <*[B](that: Parser[B]): Parser[A] = zipLeft(that)

  def map[B](f: A => B): Parser[B] =
    self.flatMap(a => Parser.succeed(f(a)))

  def or[A1 >: A](that: Parser[A1]): Parser[A1] = Parser { input =>
    self.run(input) match {
      case Right(value) => Right(value)
      case Left(_)      => that.run(input)
    }
  }

  def repeatedly: Parser[Vector[A]] =
    self
      .map(Vector(_))
      .flatMap(left => self.repeatedly.map(right => left ++ right)) or Parser.succeed(Vector.empty)

  def repeatedlySeparatedBy(seperator: Parser[_]): Parser[Vector[A]] =
    self
      .map(Vector(_))
      .flatMap(left => (seperator *> self).repeatedly.map(right => left ++ right))
      .or(Parser.succeed(Vector.empty))
}

object Parser {
  def succeed[A](a: A): Parser[A] = Parser { input =>
    Right(a, input)
  }

  def fail(message: String): Parser[Nothing] = Parser(_ => Left(message))

  val anyChar: Parser[Char] = Parser { input =>
    input.headOption match {
      case Some(char) => Right((char, input.tail))
      case None       => Left("Unexpected end of input")
    }
  }

  val digit: Parser[Char] = anyChar.suchThat(_.isDigit, char => s"Expected digit but got $char")

  def char(c: Char): Parser[Char] = anyChar.suchThat(_ == c, char => s"Expected $c but got $char")

  def string(str: String): Parser[String] = Parser { input =>
    if (input.startsWith(str)) Right((str, input.drop(str.length)))
    else Left(s"Expected $str but got ${input.take(str.length)}")
  }

  val alphabetic: Parser[String] =
    anyChar
      .suchThat(c => c.isLetter || c == '-', char => s"Expected alphabetic but got $char")
      .repeatedly
      .map(_.mkString)

  val positiveInt: Parser[Int] =
    anyChar
      .suchThat(_.isDigit, char => s"Expected digit but got $char")
      .repeatedly
      .suchThat(_.nonEmpty, _ => "Expected at least one digit")
      .map(_.mkString.toInt)

  val int: Parser[Int] = char('-').flatMap(_ => positiveInt).map(_ * -1) or positiveInt

  val whitespace: Parser[Unit] = (char(' ') or char('\n')).repeatedly.map(_ => ())

  val word: Parser[String] =
    anyChar.suchThat(_.isLetter, char => s"Expected letter but got $char").repeatedly.map(_.mkString)

  object JsonSupport {
    val nullParser: Parser[Json.JNull.type] =
      Parser.string("null").map(_ => Json.JNull)

    val trueParser: Parser[Json.JBoolean] =
      Parser.string("true").map(_ => Json.JBoolean(true))

    val falseParser: Parser[Json.JBoolean] =
      Parser.string("false").map(_ => Json.JBoolean(false))

    val booleanParser: Parser[Json.JBoolean] = trueParser or falseParser

    val numberParser: Parser[Json.JNumber] =
      for {
        sign    <- char('-').map(_.toString) or Parser.succeed("")
        intPart <- Parser.positiveInt
        decimalPart <-
          (
            Parser.char('.') *>
              Parser.positiveInt.flatMap(decimal => Parser.succeed(s".$decimal"))
          )
            .or(Parser.succeed(""))

        number = BigDecimal(s"$sign$intPart$decimalPart")
      } yield Json.JNumber(number)

    // it kinda handles some escapes but I am sure this is kinda fucked
    val stringParser: Parser[Json.JString] =
      for {
        _ <- Parser.char('"')
        str <-
          Parser
            .string("\\\"")
            .map(_ => "\"")
            .or(Parser.string("\\\\").map(_ => "\\"))
            .or(Parser.string("\\/").map(_ => "/"))
            .or(Parser.string("\\b").map(_ => "\b"))
            .or(Parser.string("\\f").map(_ => "\f"))
            .or(Parser.string("\\n").map(_ => "\n"))
            .or(Parser.string("\\r").map(_ => "\r"))
            .or(Parser.string("\\t").map(_ => "\t"))
            .or(Parser.string("\\u").flatMap(_ => Parser.string("1234").map(_ => "u")))
            .or(anyChar.suchThat(_ != '"', char => s"Expected char but got $char"))
            .repeatedly
            .map(_.mkString)
        _ <- char('"')
      } yield Json.JString(str)

    val arrayParser: Parser[Json.JArray] =
      for {
        _        <- Parser.char('[')
        _        <- Parser.whitespace
        elements <- jsonParser.repeatedlySeparatedBy(Parser.char(','))
        _        <- Parser.whitespace
        _        <- Parser.char(']')
      } yield Json.JArray(elements)

    val objectParser: Parser[Json.JObject] =
      for {
        _ <- Parser.char('{')
        _ <- Parser.whitespace
        kvs <- (for {
                 key   <- stringParser.map(_.value)
                 _     <- Parser.whitespace
                 _     <- Parser.char(':')
                 _     <- Parser.whitespace
                 value <- jsonParser
                 _     <- Parser.whitespace
               } yield (key, value)).repeatedlySeparatedBy(Parser.char(',') <* Parser.whitespace)
        _ <- Parser.whitespace
        _ <- Parser.char('}')
      } yield Json.JObject(kvs.toMap)

    def jsonParser: Parser[Json] =
      for
        _ <- Parser.whitespace
        json <- (nullParser or
                  booleanParser or
                  numberParser or
                  stringParser or
                  arrayParser or
                  objectParser)
        _ <- Parser.whitespace
      yield json
  }
}
