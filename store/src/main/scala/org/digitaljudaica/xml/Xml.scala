package org.digitaljudaica.xml

import cats.implicits._

import scala.xml.{Elem, Node}

object Xml {

  val name: Parser[String] =
    Context.inspectCurrent(_.name)

  def checkName(expected: String): Parser[Unit] = for {
    name <- name
    _  <- Parser.check(name == expected, s"Wrong element: '$name' instead of '$expected'")
  } yield ()

  def withName[A](expected: String, parser: Parser[A]): Parser[A] = for {
    _ <- checkName(expected)
    result <- parser
  } yield result

  def withInclude[A](parser: Parser[A]): Parser[A] = withInclude("include", parser)

  // TODO capture Content.Type...
  def withInclude[A](name: String, parser: Parser[A]): Parser[A] = for {
    url <- attribute.optional(name)
    result <- url.fold(parser)(Context.include(_, parser))
  } yield result

  object attribute {
    object optional {
      def apply(name: String): Parser[Option[String]] =
        Context.replaceCurrent(_.takeAttribute(name))

      def id: Parser[Option[String]] =
        apply("xml:id")

      def boolean(name: String): Parser[Option[Boolean]] =
        convert(name, resultO => Parser.pure(resultO.map(value => value == "true" || value == "yes")))

      def int(name: String): Parser[Option[Int]] =
        convert(name, resultO => Parser.toParser(resultO.map(_.toInt)))

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

  val allElements: Parser[Seq[Elem]] =
    Context.replaceCurrent(_.replaceContent(Content.takeAllElements))

  val allNodes: Parser[Seq[Node]] =
    Context.replaceCurrent(_.replaceContent(Content.takeAllNodes))

  object next {

    val element: Parser[Elem] =
      Context.replaceCurrent(_.replaceContent(Content.takeNextElement))

    val elementName: Parser[Option[String]] =
      Context.inspectCurrent(current => Content.getNextElementName(current.content))

    def elementNameIs(expected: String): Parser[Boolean] =
      elementName.map(_.contains(expected))

    def elementNameIs(expected: Option[String]): Parser[Boolean] =
      expected.fold(Parser.pure(true))(elementNameIs)
  }

  object element {

    val empty = new Element(Content.Type.Empty)

    val characters = new Element(Content.Type.Characters)

    val elements = new Element(Content.Type.Elements)

    val mixed = new Element(Content.Type.Mixed)
  }

  final class Element(contentType: Content.Type) {
    def optional[A](name: String, parser: Parser[A]): Parser[Option[A]] =
      optional(Some(name), parser)

    def required[A](name: String, parser: Parser[A]): Parser[A] =
      Parser.required(s"element '$name'", optional(name, parser))

    def all[A](name: String, parser: Parser[A]): Parser[Seq[A]] =
      all(Some(name), parser)

    def all[A](parser: Parser[A]): Parser[Seq[A]] =
      all(None, parser)

    private def optional[A](name: Option[String], parser: Parser[A]): Parser[Option[A]] = for {
      hasNext <- next.elementNameIs(name)
      result <- if (!hasNext) Parser.pure(None) else for {
        next <- next.element
        result <- Context.nested(None, next, parser, contentType)
      } yield Some(result)
    } yield result

    private def all[A](name: Option[String], parser: Parser[A]): Parser[Seq[A]] = for {
      headOption <- optional(name, parser)
      tail <- if (headOption.isEmpty) Parser.pure(Seq.empty[A]) else all(name, parser)
      result = headOption.toSeq ++ tail
    } yield result
  }

  object characters {
    val optional: Parser[Option[String]] =
      Context.replaceCurrent(_.replaceContent(Content.takeCharacters))

    val required: Parser[String] =
      Parser.required("characters", optional)
  }
}
