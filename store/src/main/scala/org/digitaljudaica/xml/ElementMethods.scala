package org.digitaljudaica.xml

import cats.implicits._

class ElementMethods(charactersAllowed: Boolean) {
  object optional {
    final def apply[A](name: String, parser: Parser[A]): Parser[Option[A]] =
      apply(name = Some(name), parser)

    final def apply[A](parser: Parser[A]): Parser[Option[A]] =
      apply(name = None, parser)

    final def apply[A](name: Option[String], parser: Parser[A]): Parser[Option[A]] = for {
      noElement <- Element.nextNested.nameIsNot(name)
      result <- if (noElement) Parser.pure(None) else for {
        next <- Context.takeNextNestedElement
        result <- Context.nested(None, next, parser, charactersAllowed)
      } yield Some(result)
    } yield result
  }

  object required {
    final def apply[A](parser: Parser[A]): Parser[A] =
      Parser.required(s"element", optional(parser))

    final def apply[A](name: String, parser: Parser[A]): Parser[A] =
      Parser.required(s"element '$name'", optional(name, parser))
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
