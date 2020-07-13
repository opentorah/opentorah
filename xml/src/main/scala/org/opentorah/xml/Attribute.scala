package org.opentorah.xml

import zio.ZIO

sealed abstract class Attribute[A](val name: String) {
  final override def toString: String = s"attribute $name"

  final def optional: Parser[Option[A]] = Context.takeAttribute(name).flatMap { value =>
    value.fold[Parser[Option[A]]](ZIO.none)(value => parseFromString(value).map(Some(_)))
  }

  final def required: Parser[A] = for {
    result <- optional
    _ <- Parser.check(result.isDefined, s"Required $this is missing")
  } yield result.get

  def parseFromString: String => Parser[A]

  def toString(value: A): String

  def withValue(value: Option[A]): Attribute.Value[A] = new Attribute.Value[A](this, value)

  def withValue(value: A): Attribute.Value[A] = withValue(Some(value))
}

object Attribute {

  final class Value[A](
    val attribute: Attribute[A],
    value: Option[A]
  ) {
    def valueToString: Option[String] = value.map(attribute.toString)
  }

  def apply(name: String): Attribute[String] = new Attribute[String](name) {
    override def parseFromString: String => Parser[String] = ZIO.succeed(_)
    override def toString(value: String): String = value
  }

  final class BooleanAttribute(name: String) extends Attribute[Boolean](name) {
    override def parseFromString: String => Parser[Boolean] = String2.boolean
    override def toString(value: Boolean): String = value.toString
    def orFalse: Parser[Boolean] = optional.map(_.getOrElse(false))
  }

  def boolean(name: String): BooleanAttribute = new BooleanAttribute(name)

  final class IntAttribute(name: String) extends Attribute[Int](name) {
    override def parseFromString: String => Parser[Int] = String2.int
    override def toString(value: Int): String = value.toString
  }

  def int(name: String): IntAttribute = new IntAttribute(name)

  final class PositiveIntAttribute(name: String) extends Attribute[Int](name) {
    override def parseFromString: String => Parser[Int] = String2.positiveInt
    override def toString(value: Int): String = value.toString
  }

  def positiveInt(name: String): PositiveIntAttribute = new PositiveIntAttribute(name)

  val id: Attribute[String] = Attribute("xml:id")

  val allAttributes: Parser[Map[String, String]] =
    Context.takeAllAttributes
}
