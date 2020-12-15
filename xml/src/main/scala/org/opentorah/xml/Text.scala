package org.opentorah.xml

final class Text extends Requireable[String] {
  override def toString: String = s"element text"

  override def optional: Parser[Option[String]] =
    Context.takeCharacters
}

object Text {

  class TextElement(elementName: String) extends Element.WithToXml[String](elementName) {
    override def toString: Error = s"text element $elementName"

    override def contentType: ContentType = ContentType.Characters

    override def parser: Parser[String] = Text().required

    override protected def antiparser: Antiparser[String] = Antiparser(
      content = value => Seq(Xml.mkText(value))
    )
  }

  def apply(): Text = new Text

  def apply(elementName: String): TextElement = new TextElement(elementName)
}
