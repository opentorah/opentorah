package org.opentorah.xml

import scala.xml.{Elem, Node}

class Element[A](
  val elementName: String,
  override val contentType: ContentType = ContentType.Elements,
  val parser: Parser[A]
) extends Parsable[A] {

  override def toString: String = s"element '$elementName'"

  final override def name2parser(elementName: String): Option[Parser[A]] =
    if (elementName == this.elementName) Some(parser) else None

  final def descendants(xml: Node): Seq[A] =
    for (xml <- XmlUtil.descendants(xml, elementName))
    yield Parser.parseDo(From.xml("descendants", xml).parse(this))
}

object Element {

  val name: Parser[String] =
    Context.lift(_.name)

  val nextName: Parser[Option[String]] =
    Context.lift(current => Content.getNextElementName(current.content))

  // TODO eliminate
  def nextNameIs(name: String): Parser[Boolean] =
    nextName.map(_.contains(name))

  val nextElement: Parser[Option[Elem]] =
    Context.liftContentModifier(Content.takeNextElement)

  val allElements: Parser[Seq[Elem]] =
    Context.liftContentModifier(Content.takeAllElements)
}
