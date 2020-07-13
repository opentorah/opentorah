package org.opentorah.xml

import scala.xml.Node

class StringElement(elementName: String) extends Element.WithToXml[String](elementName) {
  override def toString: Error = s"text element $elementName"

  override protected def contentType: ContentType = ContentType.Characters

  override protected def parser: Parser[String] = Text().required

  override protected def attributes(value: String): Seq[Attribute.Value[_]] = Seq.empty

  override protected def content(value: String): Seq[Node] =
    Seq(Xml.textNode(value))

  class Converted[B](convert: String => Parser[B]) extends Element.WithToXml[B](elementName) {
    override def toString: Error = StringElement.this.toString
    override protected def contentType: ContentType = ContentType.Characters
    override protected def parser: Parser[B] = Text().required.flatMap(convert)
    override protected def attributes(value: B): Seq[Attribute.Value[_]] = Seq.empty
    override protected def content(value: B): Seq[Node] = StringElement.this.content(value.toString)
  }

  final class BooleanElement extends Converted[Boolean](String2.boolean) {
    def orFalse: Parser[Boolean] = optional.map(_.getOrElse(false))
  }

  def boolean: BooleanElement = new BooleanElement
  def int: Element[Int] = new Converted[Int](String2.int)
  def positiveInt: Element[Int] = new Converted[Int](String2.positiveInt)
}
