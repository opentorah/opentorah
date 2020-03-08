package org.opentorah.xml

import zio.ZIO

trait AttributeLike[A] extends Optional[A]

object AttributeLike {

  abstract class Converted[A, B](
    wrapped: AttributeLike[A],
    convert: A => Parser[B]
) extends AttributeLike[B] {
    override def toString: Error = wrapped.toString

    override def optional: Parser[Option[B]] =
      // TODO simplify...
      wrapped.optional.flatMap { _.fold[Parser[Option[B]]](ZIO.none)(value => convert(value).map(Some(_))) }
  }

  final class BooleanAttributeLike(wrapped: AttributeLike[String]) extends Converted[String, Boolean](
    wrapped,
    convert = string => ZIO.succeed(string == "true" || string == "yes")
  ) {
    def orFalse: Parser[Boolean] = optional.map(_.getOrElse(false))
  }

  final class IntAttributeLike(wrapped: AttributeLike[String], mustBePositive: Boolean) extends Converted[String, Int](
    wrapped,
    convert = string => for {
      result <- Parser.effect(string.toInt)
      _ <- Parser.check(!mustBePositive || result > 0, s"Non-positive integer: $result")
    } yield result
  )

  trait StringAttributeLike extends AttributeLike[String] {
    def int: AttributeLike[Int] = new IntAttributeLike(this, mustBePositive = false)
    def positiveInt: AttributeLike[Int] = new IntAttributeLike(this, mustBePositive = true)
    def boolean: BooleanAttributeLike = new BooleanAttributeLike(this)
  }
}
