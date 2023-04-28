package breakfast

import java.util.UUID
import scala.deriving.*
import scala.compiletime.*
import scala.util.Try

final class Decoder[+A](val decode: Json => Decoder.Result[A]) { self =>
  def map[B](f: A => B): Decoder[B] =
    Decoder(json => self.decode(json).map(f))

  def mapError(f: String => String): Decoder[A] =
    Decoder(json => self.decode(json).left.map(f))

  def mapOrFail[B](f: A => Decoder.Result[B]): Decoder[B] =
    Decoder(json => self.decode(json).flatMap(f))

  def orElse[B >: A](that: => Decoder[B]): Decoder[B] =
    Decoder(json => decode(json).left.flatMap(_ => that.decode(json)))

  def flatMap[B](f: A => Decoder[B]): Decoder[B] =
    Decoder(json => self.decode(json).flatMap(a => f(a).decode(json)))

  override def toString: String = s"Decoder"
}

object Decoder {

  type Result[+A] = Either[String, A]

  def succeed[A](value: A): Decoder[A]       = Decoder(_ => Right(value))
  def fail(reason: String): Decoder[Nothing] = Decoder(_ => Left(reason))

  given Decoder[String] = Decoder {
    case Json.JString(value) => Right(value)
    case json                => Left(s"Expected JString as String, but got ${json}")
  }

  /**
   * This may involve truncation or rounding.
   */
  given Decoder[Int] = Decoder {
    case Json.JNumber(value) => Right(value.toInt)
    case json                => Left(s"Expected JNumber as Int, but got ${json}")
  }

  given Decoder[Boolean] = Decoder {
    case Json.JBoolean(value) => Right(value)
    case json                 => Left(s"Expected JBoolean as Boolean, but got ${json}")
  }

  given Decoder[BigDecimal] = Decoder {
    case Json.JNumber(value) => Right(value)
    case json                => Left(s"Expected JNumber as BigDecimal, but got ${json}")
  }

  given Decoder[Json] = Decoder(Right(_))

  given Decoder[Long] = Decoder {
    case Json.JNumber(value) => Right(value.toLong)
    case json                => Left(s"Expected JNumber as Long, but got ${json}")
  }

  /**
   * This may involve truncation or rounding.
   */
  given Decoder[Double] = Decoder {
    case Json.JNumber(value) => Right(value.toDouble)
    case json                => Left(s"Expected JNumber as Double, but got ${json}")
  }

  /**
   * This may involve truncation or rounding.
   */
  given Decoder[Float] = Decoder {
    case Json.JNumber(value) => Right(value.toFloat)
    case json                => Left(s"Expected JNumber as Float, but got ${json}")
  }

  given Decoder[Short] = Decoder {
    case Json.JNumber(value) => Right(value.toShort)
    case json                => Left(s"Expected JNumber as Short, but got ${json}")
  }

  given Decoder[CharSequence] = Decoder {
    case Json.JString(value) => Right(value)
    case json                => Left(s"Expected JString as CharSequence, but got ${json}")
  }

  given Decoder[Char] = Decoder {
    case Json.JString(value) => Right(value.charAt(0))
    case json                => Left(s"Expected JString as Char, but got ${json}")
  }

  given Decoder[BigInt] = Decoder {
    case Json.JNumber(value) => Right(value.toBigInt)
    case json                => Left(s"Expected JNumber as BigInt, but got ${json}")
  }

  given Decoder[UUID] = Decoder {
    case Json.JString(value) =>
      Try(UUID.fromString(value)).toEither.left.map(_.getMessage)
    case json => Left(s"Expected JString as UUID, but got ${json}")
  }

  inline def derived[A](using m: Mirror.Of[A]): Decoder[A] =
    inline m match {
      case p: Mirror.ProductOf[A] =>
        decoderProduct(p)
      case s: Mirror.SumOf[A] =>
        decoderSum(s)
    }

  private inline def summonDecoders[T <: Tuple]: List[Decoder[_]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[Decoder[t]] :: summonDecoders[ts]
  }

  private inline def summonLabels[L <: Tuple]: List[String] = inline erasedValue[L] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constValue[t].asInstanceOf[String] :: summonLabels[ts]
  }

  private inline def decoderProduct[A](p: Mirror.ProductOf[A]): Decoder[A] = {
    val keys: List[String] = summonLabels[p.MirroredElemLabels]
    val decoders           = summonDecoders[p.MirroredElemTypes]
    Decoder[A] {
      case Json.JObject(fields) =>
        val results = keys.zip(decoders).map { case (key, decoder) =>
          fields.get(key) match {
            case Some(value) => decoder.decode(value)
            case None        => Left(s"Missing field $key")
          }
        }
        val (failures, values) = results.partitionMap(identity)
        Either.cond(
          failures.isEmpty,
          p.fromProduct(Tuple.fromArray(values.toArray)),
          failures.mkString(", ")
        )
      case json =>
        Left(s"Expected JObject as Product, but got ${json}")
    }
  }

  private inline def decoderSum[A](s: Mirror.SumOf[A]): Decoder[A] = {
    val keys: List[String] = summonLabels[s.MirroredElemLabels]
    val decoders           = summonDecoders[s.MirroredElemTypes]
    Decoder[A] {
      case Json.JObject(fields) =>
        keys
          .zip(decoders)
          .collectFirst {
            case (key, decoder) if fields.contains(key) => decoder.decode(fields(key)).asInstanceOf[Result[A]]
          }
          .getOrElse(Left(s"Missing one of fields ${keys.mkString(", ")}"))
      case json =>
        Left(s"Expected JObject as Coproduct, but got ${json}")
    }
  }

  // decode unwrapped sums
  inline def oneOf[A](using m: Mirror.SumOf[A]): Decoder[A] = {
    val label        = constValue[m.MirroredLabel]
    val elemDecoders = summonDecoders[m.MirroredElemTypes]

    val decoder =
      if (elemDecoders.isEmpty) Decoder.fail(s"No elements for ${label.toString}")
      else if (elemDecoders.size == 1) elemDecoders.head.asInstanceOf[Decoder[A]]
      else elemDecoders.reduceLeft(_ orElse _).asInstanceOf[Decoder[A]]

    decoder.mapError(_ => s"Expected $label")
  }
}
