package org.opentorah.xml

final class Text {
  override def toString: String = s"element text"

  def optional: Parser[Option[String]] =
    Context.takeCharacters

  def required: Parser[String] = for {
    result <- optional
    _ <- Parser.check(result.isDefined, s"Required $this is missing")
  } yield result.get
}

object Text {

  def apply(): Text = new Text

  def apply(elementName: String): StringElement = new StringElement(elementName)
}
