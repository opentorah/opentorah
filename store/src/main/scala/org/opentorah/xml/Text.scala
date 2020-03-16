package org.opentorah.xml

trait Text extends StringAttributeLike

object Text {

  def apply(): StringAttributeLike = new StringAttributeLike {
    override def toString: String = s"element text"

    override def optional: Parser[Option[String]] =
      Context.takeCharacters
  }

  // TODO move to Element?
  def apply(elementName: String): StringElement = new StringElement(elementName)
}
