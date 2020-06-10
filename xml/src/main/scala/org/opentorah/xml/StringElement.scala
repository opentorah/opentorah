package org.opentorah.xml

import scala.xml.Elem

class StringElement(elementName: String) extends Element[String](
  elementName,
  ContentType.Characters,
  Text().required
) with ToXml[String] {
  override def toString: Error = s"text element $elementName"

  override def toXml(value: String): Elem =
    <elem>{value}</elem>.copy(label = elementName)

  class Converted[B](convert: String => Parser[B]) extends Element[B](
    elementName,
    contentType = ContentType.Characters,
    parser = Text().required.flatMap(convert)
  )  with ToXml[B] {
    override def toString: Error = StringElement.this.toString
    override def toXml(value: B): Elem = StringElement.this.toXml(value.toString)
  }

  final class BooleanElement extends Converted[Boolean](String2.boolean) {
    def orFalse: Parser[Boolean] = optional.map(_.getOrElse(false))
  }

  def boolean: BooleanElement = new BooleanElement
  def int: Element[Int] = new Converted[Int](String2.int)
  def positiveInt: Element[Int] = new Converted[Int](String2.positiveInt)
}
