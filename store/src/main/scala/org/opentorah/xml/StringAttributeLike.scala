package org.opentorah.xml

import zio.ZIO

trait StringAttributeLike extends AttributeLike[String] {

  class Converted[B](convert: String => Parser[B]) extends AttributeLike[B] {
    override def toString: Error = StringAttributeLike.this.toString

    // TODO simplify...
    override def optional: Parser[Option[B]] = StringAttributeLike.this.optional.flatMap { value =>
      value.fold[Parser[Option[B]]](ZIO.none)(value => convert(value).map(Some(_)))
    }
  }

  final class BooleanAttributeLike extends Converted[Boolean](String2.boolean) {
    def orFalse: Parser[Boolean] = optional.map(_.getOrElse(false))
  }

  def boolean: BooleanAttributeLike = new BooleanAttributeLike
  def int: AttributeLike[Int] = new Converted[Int](String2.int)
  def positiveInt: AttributeLike[Int] = new Converted[Int](String2.positiveInt)
}
