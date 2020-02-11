package org.digitaljudaica.xml

import cats.data.StateT
import cats.implicits._
import scala.xml.Elem

// TODO rename XML, since this is the main entry point for parsing :)
final class Context private(stack: List[Element]) {

  override def toString: String = stack.mkString("\n")

  private def isEmpty: Boolean = stack.isEmpty

  private def current: Element = stack.head

  private[xml] def getName: String = current.getName

  // TODO bring the parsers here!
  private[xml] def takeAttribute(name: String): (Context, Option[String]) = replaceCurrent(_.takeAttribute(name))

  // TODO bring the parsers here!
  private[xml] def takeCharacters: (Context, Option[String]) = replaceCurrent(_.takeCharacters)

  private def replaceCurrent[A](f: Element => (Element, A)): (Context, A) = {
    val (newCurrent, result) = f(current)
    (new Context(stack = newCurrent :: stack.tail), result)
  }

  private[xml] def getNextNestedElementName: Option[String] = current.getNextNestedElementName

  private def push(element: Element): Context = new Context(element :: stack)

  // TODO bring the parsers here!
  private[xml] def takeNextNestedElement: (Context, Elem) = replaceCurrent(_.takeNextNestedElement)

  private def pop: Context = new Context(stack.tail)

  private def checkNoMixedContent: Parser[Unit] = current.checkNoMixedContent

  private def checkNoLeftovers: Parser[Unit] = current.checkNoLeftovers

  // TODO this is a programming error - maybe just throw an exception, since it can be proven that it can't happen?
  private[xml] def checkIsEmpty: Parser[Unit] = Parse.check(isEmpty, "Non-empty context")
}

object Context {

  def parse[A](toLoad: Load.Result, parser: Parser[A]): ErrorOr[A] = run(
    toLoad match {
      case (url, what) => what match {
        case Left(exception) => Parse.lift(Left(exception.toString))
        case Right(elem) => complete(Context.nested(Some(url), elem, parser))
      }
    }
  )

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

  private[xml] def parse[A](elem: Elem, parser: Parser[A]): A =
    runA(run(complete(Context.nested(Element(url = Some("synthetic"), elem), parser))))

  private[xml] def run[A](parser: Parser[A]): ErrorOr[A] =
    parser.runA(new Context(List.empty))

  def runA[A](result: ErrorOr[A]): A =
    result.fold(error => throw new IllegalArgumentException(error), identity)

  private[xml] def nested[A](url: Option[String], elem: Elem, parser: Parser[A]): Parser[A] =
    nested(Element(url, elem), parser)

  private[xml] def nested[A](element: Element, parser: Parser[A]): Parser[A] = for {
    _ <- modify(_.push(element))
    _ <- Parse.runCheck(_.checkNoMixedContent)
    // TODO allow character content - or not...
    result <- parser
    _ <- Parse.runCheck(_.checkNoLeftovers)
    _ <- modify(_.pop)
  } yield result

  private def complete[A](parser: Parser[A]): Parser[A] = for {
    result <- parser
    _ <- Parse.runCheck(_.checkIsEmpty)
  } yield result

  private def modify(f: Context => Context): Parser[Unit] = StateT.modify[ErrorOr, Context](f)
}
