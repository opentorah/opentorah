package org.digitaljudaica.xml

import cats.implicits._
import scala.xml.Elem

final class Context private(private[xml] val stack: List[Current]) {

  override def toString: String = stack.mkString("\n")

  private def current: Current = stack.head

  private def replaceCurrent[A](f: Current => (Current, A)): (Context, A) = {
    val (newCurrent, result) = f(stack.head)
    (new Context(stack = newCurrent :: stack.tail), result)
  }

  private def push(element: Current): Context = new Context(element :: stack)

  private def pop: Context = new Context(stack.tail)

  private def checkIsEmpty(): Unit =
    if (stack.nonEmpty) throw new IllegalStateException(s"Non-empty context $this")
}

object Context {

  private[xml] def currentFrom: Parser[From] =
    Parser.inspect(_.stack.find(_.getFrom.isDefined).get.getFrom.get)

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
    inspectAndSet(_.replaceCurrent(f))

  // This seems like some kind of a StateT.liftXXX, but I do not see it...
  private def inspectAndSet[A](f: Context => (Context, A)): Parser[A] = for {
    result <- Parser.inspect(f)
    _ <- Parser.set(result._1)
  } yield result._2

  private[xml] def nested[A](
    from: Option[From],
    elem: Elem,
    parser: Parser[A],
    charactersAllowed: Boolean = false
  ): Parser[A] = for {
    _ <- Parser.modify(_.push(Current(from, elem)))
    _ <- Check(_.current.checkContent(charactersAllowed))
    result <- parser
    _ <- Check(_.current.checkNoLeftovers)
    _ <- Parser.modify(_.pop)
  } yield result

  private[xml] def complete[A](parser: Parser[A]): Parser[A] = for {
    result <- parser
    _ <- Parser.inspect(_.checkIsEmpty())
  } yield result

  private[xml] def run[A](parser: Parser[A]): ErrorOr[A] =
    parser.runA(new Context(List.empty))
}
