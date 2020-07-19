package org.opentorah.xml

import scala.xml.Node

final class Text extends Requireable[String] {
  override def toString: String = s"element text"

  override def optional: Parser[Option[String]] =
    Context.takeCharacters
}

object Text {

  class TextElement(elementName: String) extends Element.WithToXml[String](elementName) {
    override def toString: Error = s"text element $elementName"

    override protected def contentType: ContentType = ContentType.Characters

    override protected def parser: Parser[String] = Text().required

    override protected def attributes(value: String): Seq[Attribute.Value[_]] = Seq.empty

    override protected def content(value: String): Seq[Node] =
      Seq(Xml.mkText(value))
  }

  def apply(): Text = new Text

  def apply(elementName: String): TextElement = new TextElement(elementName)
}
