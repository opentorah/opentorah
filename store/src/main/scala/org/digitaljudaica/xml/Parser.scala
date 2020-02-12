package org.digitaljudaica.xml

import cats.data.StateT
import cats.implicits._

object Parser {

  def pure[A](value: A): Parser[A] = StateT.pure[ErrorOr, Context, A](value)

  def error[A](value: Error): Parser[A] = Parser.lift(Left(value))

  private[xml] def lift[A](value: ErrorOr[A]): Parser[A] = StateT.liftF[ErrorOr, Context, A](value)

  private[xml] def inspect[A](f: Context => A): Parser[A] = StateT.inspect[ErrorOr, Context, A](f)

  private[xml] def set(context: Context): Parser[Unit] = StateT.set[ErrorOr, Context](context)

  private[xml] def modify(f: Context => Context): Parser[Unit] = StateT.modify[ErrorOr, Context](f)

  def characters: Parser[Option[String]] = Context.takeCharacters

  def checkName[A](expected: String, parser: Parser[A]): Parser[A] = for {
    name <- Element.name
    _  <- Check(name == expected, s"Wrong element: $name instead of $expected")
    result <- parser
  } yield result

  def processInclude[A](attribute: String, parser: Parser[A]): Parser[A] = for {
    name <- Element.name
    url <- Attribute.optional(attribute)
    currentFrom <- Context.currentFrom
    result <- url.fold(parser)(url => currentFrom.include(url).nest(checkName(name, parser)))
  } yield result

  def wrapped[A](rootElementName: String, typeName: String, elementName: String, parser: Parser[A]): Parser[Seq[A]] =
    checkName(rootElementName, for {
      type_ <- Attribute.required("type")
      _ <- Check(type_ == typeName, s"Wrong metadata type: $type_ instead of $typeName")
      result <- Element.all(elementName, parser)
    } yield result)
}
