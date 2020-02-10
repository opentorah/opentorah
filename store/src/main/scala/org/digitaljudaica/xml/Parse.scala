package org.digitaljudaica.xml

import cats.implicits._
import cats.data.StateT

import scala.xml.Elem

object Parse {

  type Error = String

  type ErrorOr[A] = Either[Error, A]

  type Parser[A] = StateT[ErrorOr, Context, A]

  def pure[A](value: A): Parser[A] = StateT.pure[ErrorOr, Context, A](value)

//  private def error(value: Error): Parser[Unit] = lift[Unit](Left(value))

  private def lift[A](value: ErrorOr[A]): Parser[A] = StateT.liftF[ErrorOr, Context, A](value)

  private def inspect[A](f: Context => A): Parser[A] = StateT.inspect[ErrorOr, Context, A](f)

  def modify(f: Context => Context): Parser[Unit] = StateT.modify[ErrorOr, Context](f)

  private def set(context: Context): Parser[Unit] = StateT.set[ErrorOr, Context](context)

  def check(condition: Boolean, message: => String): Parser[Unit] = if (condition) lift(Right(())) else for {
    contextStr <- inspect(_.toString)
  } yield Left(message + "\n" + contextStr)

  private def required[A](what: String, parser: Parser[Option[A]]): Parser[A] = for {
    result <- parser
    _ <- check(result.isDefined, s"Required $what is missing")
  } yield result.get

  // TODO this is some kind of a StateT.lift...
  private def take[A](f: Context => (Context, A)): Parser[A] = for {
    result <- inspect(f)
    _ <- set(result._1)
  } yield result._2

  def runCheck(f: Context => Parser[Unit]): Parser[Unit] = for {
    toRun <- inspect(f)
    _ <- toRun
  } yield ()

  // --- Characters ---

  def characters: Parser[Option[String]] = take(_.takeCharacters)

  // -- Attributes ---

  def attribute(name: String): Parser[Option[String]] = take(_.takeAttribute(name))

  def requiredAttribute(name: String): Parser[String] =
    required(s"requiredAttribute '$name'", attribute(name))

  def booleanAttribute(name: String): Parser[Option[Boolean]] = for {
    resultO <- attribute(name)
    result = resultO.map(value => value == "true" || value == "yes")
  } yield result

  def requiredBooleanAttribute(name: String): Parser[Boolean] =
    required(s"boolean requiredAttribute '$name'", booleanAttribute(name))

  def intAttribute(name: String): Parser[Option[Int]] = for {
    resultO <- attribute(name)
    result <- lift[Option[Int]](
      try { Right(resultO.map(_.toInt)) }
      catch { case e: NumberFormatException => Left(e.getMessage) }) // TODO pick up Context trace...
    _ <- check(result.isEmpty || result.get > 0, s"Non-positive integer: ${result.get}")
  } yield result

  def requiredIntAttribute(name: String): Parser[Int] =
    required(s"integer requiredAttribute '$name'", intAttribute(name))

  // --- Elements ---
  val getName: Parser[String] = inspect(_.getName)

  // TODO inverse?
  def noElement(name: String): Parser[Boolean] = noElement(Some(name))

  private def noElement(name: Option[String]): Parser[Boolean] = for {
    nextNestedElementName <- inspect(_.getNextNestedElementName)
  } yield nextNestedElementName.isEmpty || name.fold(false) (name => !nextNestedElementName.contains(name))

  def checkName[A](name: String, parser: Parser[A]): Parser[A] = for {
    elementName <- getName
    _  <- check(elementName == name, s"Wrong element: $elementName instead of $name")
    result <- parser
  } yield result

  def optionalElement[A](name: String, parser: Parser[A]): Parser[Option[A]] =
    optionalElement(name = Some(name), parser)

  def optionalElement[A](parser: Parser[A]): Parser[Option[A]] =
    optionalElement(name = None, parser)

  private def optionalElement[A](name: Option[String], parser: Parser[A]): Parser[Option[A]] = for {
    noElement <- noElement(name)
    result <- if (noElement) pure(None) else for {
      next <- take(_.takeNextNestedElement)
      result <- nested(None, next, parser)
    } yield Some(result)
  } yield result

  def element[A](parser: Parser[A]): Parser[A] =
    required(s"element", optionalElement(parser))

  def element[A](name: String, parser: Parser[A]): Parser[A] =
    required(s"element '$name'", optionalElement(name, parser))

  def elements[A](name: String, parser: Parser[A]): Parser[Seq[A]] =
    elements(Some(name), parser)

  def elements[A](parser: Parser[A]): Parser[Seq[A]] =
    elements(None, parser)

  private def elements[A](name: Option[String], parser: Parser[A]): Parser[Seq[A]] = for {
    headOption <- optionalElement(name, parser)
    tail <- if (headOption.isEmpty) pure(Seq.empty[A]) else elements(name, parser)
    result = headOption.toSeq ++ tail
  } yield result

  // --- Runs ---

  private def complete[A](parser: Parser[A]): Parser[A] = for {
    result <- parser
    _ <- runCheck(_.checkIsEmpty)
  } yield result

  private def run[A](parser: Parser[A]): ErrorOr[A] =
    parser.runA(Context.empty)

  def runA[A](result: ErrorOr[A]): A =
    result.fold(error => throw new IllegalArgumentException(error), identity)

  // TODO by default, character content should not be allowed; exceptions should be explicit.

//  def loadSubresource[A](obj: AnyRef, parser: Parser[A]): Parser[A] = Parse.element(for {
//    name <- getName
//    resource <- attribute("resource")
//    result <- resource.fold(parser){ resource =>
//      Load.fromResource(Resource(obj, resource)) match {
//        case (url, what) => what match {
//          case Left(exception) => lift(Left(exception.toString))
//          case Right(elem) => nested(Some(url), elem, Parse.element(name, parser))
//        }
//      }
//    }
//  } yield result)

  def parse[A](toLoad: Load.Result, parser: Parser[A]): ErrorOr[A] = run(
    toLoad match {
      case (url, what) => what match {
        case Left(exception) => lift(Left(exception.toString))
        case Right(elem) => complete(Context.nested(Some(url), elem, parser))
      }
    }
  )

  def parse[A](elem: Elem, parser: Parser[A]): A =
    runA(run(complete(Context.nested(Element(url = Some("synthetic"), elem), parser))))
}
