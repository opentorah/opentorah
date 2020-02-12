package org.digitaljudaica.xml

import cats.implicits._

class Element(charactersAllowed: Boolean) {
  object optional {
    final def apply[A](name: String, parser: Parser[A]): Parser[Option[A]] =
      apply(name = Some(name), parser)

    final def apply[A](parser: Parser[A]): Parser[Option[A]] =
      apply(name = None, parser)

    // Only nested elements (and not the document element) can have character content allowed;
    // I do not think this is a problem :)
    final def apply[A](name: Option[String], parser: Parser[A]): Parser[Option[A]] = for {
      hasNext <- Element.nextNested.nameIs(name)
      result <- if (!hasNext) Parser.pure(None) else for {
        next <- Context.takeNextNestedElement
        result <- Context.nested(None, next, parser, charactersAllowed)
      } yield Some(result)
    } yield result
  }

  object required {
    final def apply[A](name: String, parser: Parser[A]): Parser[A] =
      Parser.required(s"element '$name'", optional(name, parser))

    final def apply[A](parser: Parser[A]): Parser[A] =
      Parser.required(s"element", optional(parser))
  }

  object all {
    final def apply[A](name: String, parser: Parser[A]): Parser[Seq[A]] =
      apply(Some(name), parser)

    final def apply[A](parser: Parser[A]): Parser[Seq[A]] =
      apply(None, parser)

    final def apply[A](name: Option[String], parser: Parser[A]): Parser[Seq[A]] = for {
      headOption <- optional(name, parser)
      tail <- if (headOption.isEmpty) Parser.pure(Seq.empty[A]) else apply(name, parser)
      result = headOption.toSeq ++ tail
    } yield result
  }
}

object Element extends Element(charactersAllowed = false) {

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

  object withCharacters extends Element(charactersAllowed = true)

  def withInclude[A](attribute: String, parser: Parser[A]): Parser[A] = for {
    url <- Attribute.optional(attribute)
    result <- url.fold(parser)(From.include(_, parser))
  } yield result
}
