package org.opentorah.xml

import zio.ZIO

trait StringAttributeLike extends AttributeLike[String] {

  abstract class Converted[B](
    wrapped: AttributeLike[String],
    convert: String => Parser[B]
  ) extends AttributeLike[B] {
    override def toString: Error = wrapped.toString

    override def optional: Parser[Option[B]] =
    // TODO simplify...
      wrapped.optional.flatMap { _.fold[Parser[Option[B]]](ZIO.none)(value => convert(value).map(Some(_))) }
  }

  final class BooleanAttributeLike(wrapped: AttributeLike[String]) extends Converted[Boolean](
    wrapped,
    convert = string => ZIO.succeed(string == "true" || string == "yes")
  ) {
    def orFalse: Parser[Boolean] = optional.map(_.getOrElse(false))
  }

  final class IntAttributeLike(wrapped: AttributeLike[String], mustBePositive: Boolean) extends Converted[Int](
    wrapped,
    convert = string => for {
      result <- Parser.effect(string.toInt)
      _ <- Parser.check(!mustBePositive || result > 0, s"Non-positive integer: $result")
    } yield result
  )

  def int: AttributeLike[Int] = new IntAttributeLike(this, mustBePositive = false)
  def positiveInt: AttributeLike[Int] = new IntAttributeLike(this, mustBePositive = true)
  def boolean: BooleanAttributeLike = new BooleanAttributeLike(this)
}
