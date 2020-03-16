package org.opentorah.xml

import zio.ZIO
import scala.xml.Elem

// TODO merge with StringAttributeLike somehow...
class StringElement(elementName: String) extends Element[String](
  elementName,
  ContentType.Text,
  Text().required
) with ToXml[String] {
  override def toString: Error = s"text element $elementName"

  override def toXml(value: String): Elem =
    <elem>{value}</elem>.copy(label = elementName)

  abstract class Converted[B](convert: String => Parser[B]) extends Element[B](
    elementName = elementName,
    contentType =  contentType,
    parser = parser.flatMap(convert)
  )  with ToXml[B] {
    override def toString: Error = StringElement.this.toString
    override def toXml(value: B): Elem = StringElement.this.toXml(value.toString)
  }

  final class BooleanElement extends Converted[Boolean](
    string => ZIO.succeed(string == "true" || string == "yes")
  ) {
    def orFalse: Parser[Boolean] = optional.map(_.getOrElse(false))
  }

  final class IntElement(mustBePositive: Boolean) extends Converted[Int](
    string => for {
      result <- Parser.effect(string.toInt)
      _ <- Parser.check(!mustBePositive || result > 0, s"Non-positive integer: $result")
    } yield result
  )

  def int: Element[Int] = new IntElement(mustBePositive = false)
  def positiveInt: Element[Int] = new IntElement(mustBePositive = true)
  def boolean: BooleanElement = new BooleanElement
}
