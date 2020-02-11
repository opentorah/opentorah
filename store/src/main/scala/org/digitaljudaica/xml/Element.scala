package org.digitaljudaica.xml

import cats.implicits._

object Element {

  val name: Parser[String] = inspect(_.getName)

  def checkName[A](expected: String, parser: Parser[A]): Parser[A] = for {
    elementName <- name
    _  <- Check(elementName == expected, s"Wrong element: $elementName instead of $expected")
    result <- parser
  } yield result

  object nextNested {
    val name: Parser[Option[String]] = inspect(_.getNextNestedElementName)

    def nameIs(expected: String): Parser[Boolean] = name.map(_.contains(expected))

    def nameIsNot(expected: String): Parser[Boolean] = nameIs(expected).map(result => !result)

    def nameIsNot(expected: Option[String]): Parser[Boolean] = for {
      nextNestedElementName <- name
    } yield nextNestedElementName.isEmpty || expected.fold(false)(expected => !nextNestedElementName.contains(expected))
  }

  object optional {
    def apply[A](name: String, parser: Parser[A]): Parser[Option[A]] =
      apply(name = Some(name), parser)

    def apply[A](parser: Parser[A]): Parser[Option[A]] =
      apply(name = None, parser)

    // TODO by default, character content should not be allowed; exceptions should be explicit.
    // Add `charactersAllowed` parameter.
    def apply[A](name: Option[String], parser: Parser[A]): Parser[Option[A]] = for {
      noElement <- nextNested.nameIsNot(name)
      result <- if (noElement) pure(None) else for {
        next <- Context.takeNextNestedElement
        result <- Context.nested(None, next, parser)
      } yield Some(result)
    } yield result
  }

  object required {
    def apply[A](parser: Parser[A]): Parser[A] =
      Check.required(s"element", Element.optional(parser))

    def apply[A](name: String, parser: Parser[A]): Parser[A] =
      Check.required(s"element '$name'", Element.optional(name, parser))
  }

  object all {
    def apply[A](name: String, parser: Parser[A]): Parser[Seq[A]] =
      apply(Some(name), parser)

    def apply[A](parser: Parser[A]): Parser[Seq[A]] =
      apply(None, parser)

    def apply[A](name: Option[String], parser: Parser[A]): Parser[Seq[A]] = for {
      headOption <- Element.optional(name, parser)
      tail <- if (headOption.isEmpty) pure(Seq.empty[A]) else Element.all(name, parser)
      result = headOption.toSeq ++ tail
    } yield result
  }
}
