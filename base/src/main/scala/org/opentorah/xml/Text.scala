package org.opentorah.xml

import org.opentorah.util.Effects

final class Text {
  override def toString: String = s"element text"

  private def optionalParser: Parser[Option[String]] = Context.takeCharacters

  def optional: Parsable[Option[String]] = new Parsable[Option[String]] {
    override protected def parser: Parser[Option[String]] = optionalParser
    override def unparser: Unparser[Option[String]] = Unparser(
      content = value => value.toSeq.map(ScalaXml.mkText)
    )
  }

  def required: Parsable[String] = new Parsable[String] {
    override protected def parser: Parser[String] = Effects.required(optionalParser, this)
    override def unparser: Unparser[String] = Unparser(
      content = value => Seq(ScalaXml.mkText(value))
    )
  }
}

object Text {

  def apply(): Text = new Text

  final class TextElement(elementName: String) extends Element[String](elementName) {
    override def toString: String = s"text element $elementName"
    override def contentType: ContentType = ContentType.Characters
    override def contentParsable: Parsable[String] = Text().required
  }

  def apply(elementName: String): TextElement = new TextElement(elementName)
}
