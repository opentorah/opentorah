package org.opentorah.xml

import scala.xml.{Elem, Node}

class Element[A](
  elementName: String,
  contentType: ContentType,
  parser: Parser[A]
) extends Repeatable.WithElementName[A](elementName) {

  override def toString: String = s"element $elementName"

  final override protected def parse(elem: Elem): Parser[A] =
    Context.nested(None, elem, contentType, parser)
}

object Element {
  def apply[A](name: String, contentType: ContentType, parser: Parser[A]): Repeatable[A] =
    new Element(name, contentType, parser)

  def apply[A](name: String, parser: Parser[A]): Repeatable[A] =
    apply(name, ContentType.Elements, parser)

  def allNodes(name: String): Repeatable[Seq[Node]] =
    new Element(name, ContentType.Mixed, Parser.allNodes)

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
