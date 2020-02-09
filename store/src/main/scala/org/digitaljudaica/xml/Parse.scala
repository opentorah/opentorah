package org.digitaljudaica.xml

import cats.implicits._
import cats.data.StateT
import scala.xml.Elem

object Parse {
  type Error = String
  type ErrorOr[A] = Either[Error, A]
  type Parser[A] = StateT[ErrorOr, Context, A]

  private def pure[A](value: A): Parser[A] = StateT.pure[ErrorOr, Context, A](value)

  private def lift[A](value: ErrorOr[A]): Parser[A] = StateT.liftF[ErrorOr, Context, A](value)

  private def inspect[A](f: Context => A): Parser[A] = StateT.inspect[ErrorOr, Context, A](f)

  private def modify(f: Context => Context): Parser[Unit] = StateT.modify[ErrorOr, Context](f)

  def check(condition: Boolean, message: => String): Parser[Unit] =
    lift(if (condition) Right(()) else Left(message))

  private val getName: Parser[String] = inspect(_.getName)

  private val getElements: Parser[Seq[Elem]] = inspect(_.getElements)

  private val getCharacters: Parser[Option[String]] = inspect(_.getCharacters)

  private val getNextNestedElementName: Parser[Option[String]] = inspect(_.getNextNestedElementName)

  private def required[A](what: String, name: String, parser: String => Parser[Option[A]]): Parser[A] = for {
    result <- parser(name)
    _ <- check(result.isDefined, s"Required $what '$name' is missing")
  } yield result.get

  def optionalAttribute(name: String): Parser[Option[String]] = for {
    result <- inspect(_.getAttribute(name))
    _ <- modify(_.forgetAttribute(name))
  } yield result

  def attribute(name: String): Parser[String] =
    required(s"attribute", name, optionalAttribute)

  def optionalBooleanAttribute(name: String): Parser[Option[Boolean]] = for {
    resultO <- optionalAttribute(name)
    result = resultO.map(value => value == "true" || value == "yes")
  } yield result

  def optionalIntAttribute(name: String): Parser[Option[Int]] = for {
    resultO <- optionalAttribute(name)
    result = resultO.map(_.toInt) // TODO turn exception into our error
    _ <- check(result.isEmpty || result.get > 0, s"Non-positive integer: ${result.get}")
  } yield result

  def intAttribute(name: String): Parser[Int] =
    required(s"attribute", name, optionalIntAttribute)

  def optionalCharacters: Parser[Option[String]] = for {
    result <- getCharacters
    _ <- modify(_.forgetCharacters)
  } yield result

  val checkNoMixedContent: Parser[Unit] = for {
    elements <- getElements
    characters <- getCharacters
    _ <- check(elements.isEmpty || characters.isEmpty, s"Mixed content: [${characters.get}] $elements")
  } yield ()

  val checkNothingLeftBehind: Parser[Unit] = for {
    // no left-over elements
    elementsAfter <- getElements
    _ <- check(elementsAfter.isEmpty, s"Unparsed elements: $elementsAfter")

    // no left-over characters
    charactersAfter <- getCharacters
    _ <- check(charactersAfter.isEmpty, s"Unparsed characters: ${charactersAfter.get}")
  } yield ()

  def checkName(name: String): Parser[Unit] = for {
    elementName <- getName
    _  <- check(elementName == name, s"Wrong element: $elementName instead of $name")
  } yield ()

  def optionalElement[A](name: String, parser: Parser[A]): Parser[Option[A]] = for {
    nextNestedElementName <- getNextNestedElementName
    result <- if (!nextNestedElementName.contains(name)) pure(None) else for {
      _ <- modify(_.pushNextNestedElement)
      _ <- checkName(name) // just for fun - shouldn't be needed :)
      _ <- checkNoMixedContent
      result <- parser
      _ <- checkNothingLeftBehind
      _ <- modify(_.pop)
    } yield Some(result)
  } yield result

  def element[A](name: String, parser: Parser[A]): Parser[A] =
    required(s"element", name, optionalElement(_, parser))

  def load(toLoad: Load.Result): Parser[Unit] = {
    val (url, what) = toLoad
    what match {
      case Left(exception) => lift(Left(exception.toString))
      case Right(elem) => for {
        _ <- modify(_.push(Some(url), elem))
        _ <- checkNoMixedContent
      } yield ()
    }
  }

  def parse[A](toLoad: Load.Result, parser: Parser[A]): ErrorOr[A] = {
    val result: Parser[A] = for {
      _ <- load(toLoad)
      result <- parser
    } yield result
    result.runA(Context(List.empty))
  }
}
