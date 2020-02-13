package org.digitaljudaica.xml

import java.net.URL

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

  private[xml] def include[A](url: String, parser: Parser[A]): Parser[A] = for {
    name <- Element.name
    currentFrom <- currentFrom
    from <- Parser.toParser(From.url(currentFrom.url.fold(new URL(url))(new URL(_, url))))
    result <- nested(from, Element.withName(name, parser))
  } yield result

  private[xml] def nested[A](from: From, parser: Parser[A]): Parser[A] = from.load match {
    case Right(elem) => nested(Some(from), elem, parser, charactersAllowed = false, elementsAllowed = true)
    case Left(error) => Parser.error(error)
  }

  private[xml] def nested[A](
    from: Option[From],
    elem: Elem,
    parser: Parser[A],
    charactersAllowed: Boolean,
    elementsAllowed: Boolean
  ): Parser[A] = for {
    _ <- Parser.modify(_.push(Current(from, elem)))
    _ <- Parser.eval(_.current.checkContent(charactersAllowed, elementsAllowed))
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
