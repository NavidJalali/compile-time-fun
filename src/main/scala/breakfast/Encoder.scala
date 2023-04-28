package breakfast

import java.util.UUID
import scala.deriving.Mirror
import scala.compiletime.*

final class Encoder[A](val encode: A => Json) extends AnyVal { self =>
  def contramap[B](transformation: B => A): Encoder[B] =
    Encoder[B](transformation andThen self.encode)

  def mapJson(f: Json => Json): Encoder[A] =
    Encoder[A](self.encode andThen f)
}

object Encoder {
  given string: Encoder[String] = Encoder[String](Json.JString.apply)

  given int: Encoder[Int] = Encoder[Int](i => Json.JNumber.apply(BigDecimal(i)))

  given bigDecimal: Encoder[BigDecimal] = Encoder[BigDecimal](Json.JNumber.apply)

  given boolean: Encoder[Boolean] = Encoder[Boolean](Json.JBoolean.apply)

  given json: Encoder[Json] = Encoder[Json](identity)

  given long: Encoder[Long] = Encoder[Long](i => Json.JNumber.apply(BigDecimal(i)))

  given double: Encoder[Double] = Encoder[Double](i => Json.JNumber.apply(BigDecimal(i)))

  given char: Encoder[Char] = Encoder[Char](c => Json.JString(c.toString))

  given bigInt: Encoder[BigInt] = Encoder[BigInt](i => Json.JNumber.apply(BigDecimal(i)))

  given uuid: Encoder[UUID] = Encoder[UUID](u => Json.JString(u.toString))

  given optional[A](using encoder: Encoder[A]): Encoder[Option[A]] =
    Encoder {
      case Some(a) => encoder.encode(a)
      case None    => Json.JNull
    }

  inline def derived[A](using mirror: Mirror.Of[A]): Encoder[A] =
    inline mirror match
      case m: Mirror.ProductOf[A] => derivedProduct(m)
      case m: Mirror.SumOf[A]     => derivedSum(m)

  private inline def derivedProduct[A](mirror: Mirror.ProductOf[A]): Encoder[A] =
    val keys: List[String] = summonLabels[mirror.MirroredElemLabels]
    val valueEncoders: List[Encoder[_]] = summonEncoders[mirror.MirroredElemTypes]

    Encoder[A](a =>
      val values: List[Any] = a.asInstanceOf[Product].productIterator.toList

      val jsonValues: List[Json] = values.zip(valueEncoders).map {
        case (value, encoder) => encoder.asInstanceOf[Encoder[Any]].encode(value)
      }

      Json.JObject(keys.zip(jsonValues).toMap)
    )

  private inline def derivedSum[A](mirror: Mirror.SumOf[A]): Encoder[A] =
    val keys: List[String] = summonLabels[mirror.MirroredElemLabels]
    val encoders = summonEncoders[mirror.MirroredElemTypes]

    Encoder[A](a =>
      val index = mirror.ordinal(a)
      val key = keys(index)
      val value = encoders(index).asInstanceOf[Encoder[Any]].encode(a)
      Json.JObject(Map(key -> value))
    )

  // Encode sums but without their key
  private inline def oneOf[A](s: Mirror.SumOf[A], encoders: List[Encoder[_]]): Encoder[A] =
    Encoder[A] { a =>
      val ord = s.ordinal(a)
      encoders(ord).asInstanceOf[Encoder[Any]].encode(a)
    }

  private inline def summonEncoders[T <: Tuple]: List[Encoder[_]] =
    inline erasedValue[T] match
      case EmptyTuple => Nil
      case _: (head *: tail) => summonInline[Encoder[head]] :: summonEncoders[tail]

  private inline def summonLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case EmptyTuple => Nil
      case _: (head *: tail) => constValue[head].toString :: summonLabels[tail]
}
