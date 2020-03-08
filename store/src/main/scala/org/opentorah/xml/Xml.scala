package org.opentorah.xml

import java.net.URL
import scala.xml.{Elem, Node}

// TODO fold into parser...
object Xml {

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

  def withInclude[A](parser: Parser[A]): Parser[A] =
    withInclude("include", ContentType.Elements, parser)

  def withInclude[A](attributeName: String, contentType: ContentType, parser: Parser[A]): Parser[A] = for {
    url <- Attribute(attributeName).optional
    result <- url.fold(parser) { url => for {
      name <- name
      currentFromUrl <- Context.currentFromUrl
      from <- Parser.effect(From.url(currentFromUrl.fold(new URL(url))(new URL(_, url))))
      result <- nested(from, contentType, withName(name, parser))
    } yield result}
  } yield result

  def allAttributes: Parser[Map[String, String]] =
    Context.liftCurrentModifier(Current.takeAllAttributes)

  val nextName: Parser[Option[String]] =
    Context.lift(current => Content.getNextElementName(current.content))

  def nextNameIs(name: String): Parser[Boolean] =
    nextName.map(_.contains(name))

  val nextElement: Parser[Option[Elem]] =
    Context.liftContentModifier(Content.takeNextElement)

  val allElements: Parser[Seq[Elem]] =
    Context.liftContentModifier(Content.takeAllElements)

  val allNodes: Parser[Seq[Node]] =
    Context.liftContentModifier(Content.takeAllNodes)

  def nested[A](name: String, xml: Elem, contentType: ContentType, parser: Parser[A]): Parser[A] =
    nested(From.xml(name, xml), contentType, parser)

  def nested[A](from: From, contentType: ContentType, parser: Parser[A]): Parser[A] =
    Context.nested(from, contentType, parser)
}
