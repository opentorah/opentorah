package org.digitaljudaica.xml

import cats.implicits._

class Element(charactersAllowed: Boolean, elementsAllowed: Boolean) {
  final def optional[A](name: String, parser: Parser[A]): Parser[Option[A]] =
    optional(Some(name), parser)

  final def required[A](name: String, parser: Parser[A]): Parser[A] =
    Parser.required(s"element '$name'", optional(name, parser))

  final def all[A](name: String, parser: Parser[A]): Parser[Seq[A]] =
    all(Some(name), parser)

  final def all[A](parser: Parser[A]): Parser[Seq[A]] =
    all(None, parser)

  private final def optional[A](name: Option[String], parser: Parser[A]): Parser[Option[A]] = for {
    hasNext <- Element.nextNested.nameIs(name)
    result <- if (!hasNext) Parser.pure(None) else for {
      next <- Context.takeNextNestedElement
      result <- Context.nested(None, next, parser, charactersAllowed, elementsAllowed)
    } yield Some(result)
  } yield result

  private final def all[A](name: Option[String], parser: Parser[A]): Parser[Seq[A]] = for {
    headOption <- optional(name, parser)
    tail <- if (headOption.isEmpty) Parser.pure(Seq.empty[A]) else all(name, parser)
    result = headOption.toSeq ++ tail
  } yield result
}

// Defaults; same are used in the top-level nested() call.
object Element extends Element(charactersAllowed = false, elementsAllowed = true) {

  val name: Parser[String] = Context.getName

  def withName[A](expected: String, parser: Parser[A]): Parser[A] = for {
    name <- name
    _  <- Parser.check(name == expected, s"Wrong element: '$name' instead of '$expected'")
    result <- parser
  } yield result

  object nextNested {
    val name: Parser[Option[String]] =
      Context.getNextNestedElementName

    def nameIs(expected: String): Parser[Boolean] =
      name.map(_.contains(expected))

    def nameIs(expected: Option[String]): Parser[Boolean] =
      expected.fold(Parser.pure(true))(nameIs)
  }

  object empty extends Element(charactersAllowed = false, elementsAllowed = false)
  object withCharacters extends Element(charactersAllowed = true, elementsAllowed = true)
  object withCharactersOnly extends Element(charactersAllowed = true, elementsAllowed = false)

  def withInclude[A](parser: Parser[A]): Parser[A] = withInclude("include", parser)

  def withInclude[A](attribute: String, parser: Parser[A]): Parser[A] = for {
    url <- Attribute.optional(attribute)
    result <- url.fold(parser)(Context.include(_, parser))
  } yield result
}
