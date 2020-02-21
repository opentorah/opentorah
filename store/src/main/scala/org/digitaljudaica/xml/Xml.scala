package org.digitaljudaica.xml

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
        Context.lift(Current.takeAttribute(name))

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

  def allAttributes: Parser[Map[String, String]] =
    Context.lift(Current.takeAllAttributes)

  val allNodes: Parser[Seq[Node]] =
    lift(Content.takeAllNodes)

  val nextName: Parser[Option[String]] =
    Context.inspectCurrent(current => Content.getNextElementName(current.content))

  def nextNameIs(name: String): Parser[Boolean] =
    nextName.map(_.contains(name))

  val optional: Parser[Option[Elem]] =
    lift(Content.takeNextElement)

  val required: Parser[Elem] =
    Parser.required(s"element", optional)

  def optional(name: String): Parser[Option[Elem]] = for {
    has <- nextNameIs(name)
    result <- if (!has) Parser.pure(None) else optional
  } yield result

  def required(name: String): Parser[Elem] =
    Parser.required(s"element $name", optional(name))

  def required[A](name: String, contentType: ContentType, parser: Parser[A]): Parser[A] =
    Parser.required(s"element '$name'", optional(name, contentType, parser))

  def required[A](name: String, parser: Parser[A]): Parser[A] =
    required(name, ContentType.Elements, parser)

  def required[A](contentType: ContentType, parser: Parser[A]): Parser[A] =
    Parser.required(s"element", optional(contentType, parser))

  def required[A](parser: Parser[A]): Parser[A] =
    required(ContentType.Elements, parser)

  def optional[A](parser: Parser[A]): Parser[Option[A]] =
    optional(ContentType.Elements, parser)

  def optional[A](name: String, contentType: ContentType, parser: Parser[A]): Parser[Option[A]] =
    optional(Some(name), contentType, parser)

  def optional[A](name: String, parser: Parser[A]): Parser[Option[A]] =
    optional(Some(name), ContentType.Elements, parser)

  def optional[A](contentType: ContentType, parser: Parser[A]): Parser[Option[A]] =
    optional(None, contentType, parser)

  private def optional[A](name: Option[String], contentType: ContentType, parser: Parser[A]): Parser[Option[A]] = for {
    nextElementName <- nextName
    hasNext = name.fold(nextElementName.isDefined)(nextElementName.contains)
    result <- if (!hasNext) Parser.pure(None) else for {
      next <- required
      result <- Context.nested(None, next, parser, contentType)
    } yield Some(result)
  } yield result

  val all: Parser[Seq[Elem]] =
    lift(Content.takeAllElements)

  def all[A](name: String, contentType: ContentType, parser: Parser[A]): Parser[Seq[A]] =
    all(Some(name), contentType, parser)

  def all[A](name: String, parser: Parser[A]): Parser[Seq[A]] =
    all(Some(name), ContentType.Elements, parser)

  def all[A](contentType: ContentType, parser: Parser[A]): Parser[Seq[A]] =
    all(None, contentType, parser)

  def all[A](parser: Parser[A]): Parser[Seq[A]] =
    all(None, ContentType.Elements, parser)

  private def all[A](name: Option[String], contentType: ContentType, parser: Parser[A]): Parser[Seq[A]] = for {
    headOption <- optional(name, contentType, parser)
    tail <- if (headOption.isEmpty) Parser.pure(Seq.empty[A]) else all(name, contentType, parser)
    result = headOption.toSeq ++ tail
  } yield result

  object text {
    val optional: Parser[Option[String]] =
      lift(Content.takeCharacters)

    val required: Parser[String] =
      Parser.required("text", optional)
  }

  private def lift[A]: Content.Modifier[A] => Parser[A] =
    Context.lift[A] compose Current.lift[A]
}
