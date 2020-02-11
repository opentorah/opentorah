package org.digitaljudaica.xml

import cats.implicits._
import scala.xml.Elem

final class Context private(stack: List[Current]) {

  override def toString: String = stack.mkString("\n")

  private def isEmpty: Boolean = stack.isEmpty

  private def current: Current = stack.head

  private[xml] def getName: String = current.getName

  private def replaceCurrent[A](f: Current => (Current, A)): (Context, A) = {
    val (newCurrent, result) = f(current)
    (new Context(stack = newCurrent :: stack.tail), result)
  }

  private[xml] def getNextNestedElementName: Option[String] = current.getNextNestedElementName

  private def push(element: Current): Context = new Context(element :: stack)

  private def pop: Context = new Context(stack.tail)

  private def checkNoMixedContent: Parser[Unit] = current.checkNoMixedContent

  private def checkNoLeftovers: Parser[Unit] = current.checkNoLeftovers

  private[xml] def checkIsEmpty(): Unit =
    if (!isEmpty) throw new IllegalStateException(s"Non-empty context $this")
}

object Context {

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

  private[xml] def run[A](parser: Parser[A]): ErrorOr[A] =
    parser.runA(new Context(List.empty))

  private[xml] def takeAttribute(name: String): Parser[Option[String]] =
    take(_.takeAttribute(name))

  private[xml] def takeCharacters: Parser[Option[String]] =
    take(_.takeCharacters)

  private[xml] def takeNextNestedElement: Parser[Elem] =
    take(_.takeNextNestedElement)

  private def take[A](f: Current => (Current, A)): Parser[A] =
    inspectAndSet(_.replaceCurrent(f))

  // TODO this is some kind of a StateT.lift...
  private def inspectAndSet[A](f: Context => (Context, A)): Parser[A] = for {
    result <- inspect(f)
    _ <- set(result._1)
  } yield result._2

  private[xml] def nested[A](from: Option[From], elem: Elem, parser: Parser[A]): Parser[A] = for {
    _ <- modify(_.push(Current(from, elem)))
    _ <- Check(_.checkNoMixedContent)
    // TODO allow character content - or not...
    result <- parser
    _ <- Check(_.checkNoLeftovers)
    _ <- modify(_.pop)
  } yield result

  private[xml] def complete[A](parser: Parser[A]): Parser[A] = for {
    result <- parser
    _ <- inspect(_.checkIsEmpty())
  } yield result
}
