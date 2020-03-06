package org.opentorah.xml

import java.net.URL
import zio.ZIO
import scala.xml.{Elem, Node}

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
    url <- attribute.optional(attributeName)
    result <- url.fold(parser) { url => for {
      name <- name
      currentFromUrl <- Context.currentFromUrl
      from <- Parser.effect(From.url(currentFromUrl.fold(new URL(url))(new URL(_, url))))
      result <- nested(from, contentType, withName(name, parser))
    } yield result}
  } yield result

  object attribute {
    object optional {
      def apply(name: String): Parser[Option[String]] =
        Context.liftCurrentModifier(Current.takeAttribute(name))

      def id: Parser[Option[String]] =
        apply("xml:id")

      def boolean(name: String): Parser[Option[Boolean]] =
        convert(name, resultO => ZIO.succeed(resultO.map(value => value == "true" || value == "yes")))

      def int(name: String): Parser[Option[Int]] =
        convert(name, resultO => Parser.effect(resultO.map(_.toInt)))

      // TODO expose and reuse for text...
      private def convert[A](name: String, f: Option[String] => Parser[A]): Parser[A] = for {
        resultO <- optional(name)
        result <- f(resultO)
      }  yield result

      def booleanOrFalse(name: String): Parser[Boolean] = for {
        result <- boolean(name)
      } yield result.getOrElse(false)

      def positiveInt(name: String): Parser[Option[Int]] = for {
        result <- int(name)
        _ <- Parser.check(result.fold(true)(_ > 0), s"Non-positive integer: ${result.get}")
      } yield result
    }

    object required {
      def apply(name: String): Parser[String] =
        Parser.required(s"attribute '$name'", optional(name))

      def id: Parser[String] =
        apply("xml:id")

      def boolean(name: String): Parser[Boolean] =
        Parser.required(s"boolean attribute '$name'", optional.boolean(name))

      def int(name: String): Parser[Int] =
        Parser.required(s"integer attribute '$name'", optional.int(name))

      def positiveInt(name: String): Parser[Int] =
        Parser.required(s"positive integer attribute '$name'", optional.positiveInt(name))
    }
  }

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

  object text {
    val optional: Parser[Option[String]] =
      Context.liftContentModifier(Content.takeCharacters)

    val required: Parser[String] =
      Parser.required("text", optional)

    def optional(name: String): Parser[Option[String]] =
      Element(name, ContentType.Text, required).optional

    def required(name: String): Parser[String] =
      Element(name, ContentType.Text, required).required
  }

  def nested[A](name: String, xml: Elem, contentType: ContentType, parser: Parser[A]): Parser[A] =
    nested(From.xml(name, xml), contentType, parser)

  def nested[A](from: From, contentType: ContentType, parser: Parser[A]): Parser[A] =
    Context.nested(from, contentType, parser)
}
