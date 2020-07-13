package org.opentorah.xml

final class Text extends Requireable[String] {
  override def toString: String = s"element text"

  override def optional: Parser[Option[String]] =
    Context.takeCharacters
}

object Text {

  def apply(): Text = new Text

  def apply(elementName: String): StringElement = new StringElement(elementName)
}
