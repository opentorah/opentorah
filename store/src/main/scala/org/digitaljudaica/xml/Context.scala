package org.digitaljudaica.xml

import cats.implicits._
import scala.xml.Elem

final class Context private(stack: List[Current]) {

  override def toString: String = stack.mkString("\n")

  private def current: Current = stack.head

  private def currentFrom: From = stack.find(_.getFrom.isDefined).get.getFrom.get

  private def replaceCurrent[A](f: Current => (Current, A)): (Context, A) = f(stack.head)
    .bimap(newCurrent => new Context(stack = newCurrent :: stack.tail), identity)

  private def push(element: Current): Context = new Context(element :: stack)

  private def pop: Context = new Context(stack.tail)

  private def checkIsEmpty(): Unit =
    if (stack.nonEmpty) throw new IllegalStateException(s"Non-empty context $this")
}

object Context {

  private[xml] def currentFrom: Parser[From] =
    Parser.inspect(_.currentFrom)

  private[xml] def getName: Parser[String] =
    Parser.inspect(_.current.getName)

  private[xml] def getNextNestedElementName: Parser[Option[String]] =
    Parser.inspect(_.current.getNextNestedElementName)

  private[xml] def takeAttribute(name: String): Parser[Option[String]] =
    take(_.takeAttribute(name))

  private[xml] def takeCharacters: Parser[Option[String]] =
    take(_.takeCharacters)

  private[xml] def takeNextNestedElement: Parser[Elem] =
    take(_.takeNextNestedElement)

  private def take[A](f: Current => (Current, A)): Parser[A] =
    Parser.inspectAndSet(_.replaceCurrent(f))

  private[xml] def nested[A](
    from: Option[From],
    elem: Elem,
    parser: Parser[A],
    charactersAllowed: Boolean
  ): Parser[A] = for {
    _ <- Parser.modify(_.push(Current(from, elem)))
    _ <- Parser.eval(_.current.checkContent(charactersAllowed))
    result <- parser
    _ <- Parser.eval(_.current.checkNoLeftovers)
    _ <- Parser.modify(_.pop)
  } yield result

  private[xml] def parse[A](parser: Parser[A]): ErrorOr[A] = {
    val result = for {
      result <- parser
      _ <- Parser.inspect(_.checkIsEmpty())
    } yield result

    result.runA(new Context(List.empty))
  }
}
