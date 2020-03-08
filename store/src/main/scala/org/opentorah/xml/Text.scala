package org.opentorah.xml

trait Text extends AttributeLike.StringAttributeLike

object Text {

  def apply(): AttributeLike.StringAttributeLike = new AttributeLike.StringAttributeLike {
    override def toString: String = s"element text"

    override def optional: Parser[Option[String]] =
      Context.liftContentModifier(Content.takeCharacters)
  }

  def apply(elementName: String): AttributeLike.StringAttributeLike = new AttributeLike.StringAttributeLike {
    override def toString: Error = s"text element $elementName"

    override def optional: Parser[Option[String]] =
      Element(elementName, ContentType.Text, Text().required).optional
  }
}
