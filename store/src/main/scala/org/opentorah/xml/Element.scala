package org.opentorah.xml

import scala.xml.{Elem, Node}

abstract class Element[A](
  val elementName: String,
  override val contentType: ContentType = ContentType.Elements,
  val parser: Parser[A]
) extends Parsable[A] {

  override def toString: String = s"element $elementName"

  final override def name2parser(elementName: String): Option[Parser[A]] =
    if (elementName == this.elementName) Some(parser) else None


  // TODO move elsewhere?
  final def parse(from: From): Parser[A] =
    from.parse(contentType, Element.withName(elementName, parser))

  // TODO move elsewhere
  final def descendants(xml: Node): Seq[A] =
    XmlUtil.descendants(xml, elementName).map(xml =>
      Parser.parseDo(parse(From.xml(xml)))
    )
}

object Element {

  // TODO use converting wrapper?
//  def allNodes(name: String): Element[Seq[Node]] =
//    new Element(name, ContentType.Mixed, Parser.allNodes)

  val name: Parser[String] =
    Context.lift(_.name)

  def checkName(expected: String): Parser[Unit] = for {
    name <- name
    _  <- Parser.check(name == expected, s"Wrong element: '$name' instead of '$expected'")
  } yield ()

  def withName[A](expected: String, parser: Parser[A]): Parser[A] = for {
    _ <- checkName(expected)
    result <- parser
  } yield result

  val nextName: Parser[Option[String]] =
    Context.lift(current => Content.getNextElementName(current.content))

  def nextNameIs(name: String): Parser[Boolean] =
    nextName.map(_.contains(name))

  val nextElement: Parser[Option[Elem]] =
    Context.liftContentModifier(Content.takeNextElement)

  val allElements: Parser[Seq[Elem]] =
    Context.liftContentModifier(Content.takeAllElements)
}
