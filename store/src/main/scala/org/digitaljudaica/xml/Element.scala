package org.digitaljudaica.xml

import cats.implicits._

object Element {

  val name: Parser[String] = Context.getName

  object nextNested {
    val name: Parser[Option[String]] = Context.getNextNestedElementName

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

    def apply[A](name: Option[String], parser: Parser[A], charactersAllowed: Boolean = false): Parser[Option[A]] = for {
      noElement <- nextNested.nameIsNot(name)
      result <- if (noElement) Parser.pure(None) else for {
        next <- Context.takeNextNestedElement
        result <- Context.nested(None, next, parser, charactersAllowed)
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

    def apply[A](name: Option[String], parser: Parser[A], charactersAllowed: Boolean = false): Parser[Seq[A]] = for {
      headOption <- Element.optional(name, parser, charactersAllowed)
      tail <- if (headOption.isEmpty) Parser.pure(Seq.empty[A]) else Element.all(name, parser, charactersAllowed)
      result = headOption.toSeq ++ tail
    } yield result
  }

  object characters {
    object optional {
      def apply[A](name: String, parser: Parser[A]): Parser[Option[A]] =
        apply(name = Some(name), parser)

      def apply[A](parser: Parser[A]): Parser[Option[A]] =
        apply(name = None, parser)

      def apply[A](name: Option[String], parser: Parser[A]): Parser[Option[A]] =
        Element.optional(name, parser, charactersAllowed = true)
    }

    object required {
      def apply[A](parser: Parser[A]): Parser[A] =
        Check.required(s"element", Element.characters.optional(parser))

      def apply[A](name: String, parser: Parser[A]): Parser[A] =
        Check.required(s"element '$name'", Element.characters.optional(name, parser))
    }

    object all {
      def apply[A](name: String, parser: Parser[A]): Parser[Seq[A]] =
        apply(Some(name), parser)

      def apply[A](parser: Parser[A]): Parser[Seq[A]] =
        apply(None, parser)

      def apply[A](name: Option[String], parser: Parser[A]): Parser[Seq[A]] =
        Element.all(name, parser, charactersAllowed = true)
    }
  }
}
