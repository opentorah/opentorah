package org.opentorah.xml

import zio.ZIO
import scala.xml.Elem

// TODO merge with StringAttributeLike somehow...
class StringElement(elementName: String) extends Element[String](
  elementName,
  ContentType.Text,
  Text().required
) {
  override def toString: Error = s"text element $elementName"

  override def toXml(value: String): Elem =
    <elem>{value}</elem>.copy(label = elementName)

  abstract class Converted[B](
    wrapped: Element[String],
    convert: String => Parser[B]
  ) extends Element[B](
    elementName = wrapped.elementName,
    contentType = wrapped.contentType,
    parser = wrapped.parser.flatMap(convert)
  ) {
    override def toString: Error = wrapped.toString

    override def toXml(value: B): Elem = wrapped.toXml(value.toString)
  }

  final class BooleanElement(wrapped: Element[String]) extends Converted[Boolean](
    wrapped,
    convert = string => ZIO.succeed(string == "true" || string == "yes")
  ) {
    def orFalse: Parser[Boolean] = optional.map(_.getOrElse(false))
  }

  final class IntElement(wrapped: Element[String], mustBePositive: Boolean) extends Converted[Int](
    wrapped,
    convert = string => for {
      result <- Parser.effect(string.toInt)
      _ <- Parser.check(!mustBePositive || result > 0, s"Non-positive integer: $result")
    } yield result
  )

  def int: Element[Int] = new IntElement(this, mustBePositive = false)
  def positiveInt: Element[Int] = new IntElement(this, mustBePositive = true)
  def boolean: BooleanElement = new BooleanElement(this)
}
