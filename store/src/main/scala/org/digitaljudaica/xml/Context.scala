package org.digitaljudaica.xml

import cats.implicits._
import org.digitaljudaica.xml.Parse.Parser
import scala.xml.Elem

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

  private[xml] def checkIsEmpty: Parser[Unit] = Parse.check(isEmpty, "Non-empty context")
}

object Context {
  private[xml] def empty: Context = new Context(List.empty)

  private[xml] def nested[A](url: Option[String], elem: Elem, parser: Parser[A]): Parser[A] =
    nested(Element(url, elem), parser)

  private[xml] def nested[A](element: Element, parser: Parser[A]): Parser[A] = for {
    _ <- Parse.modify(_.push(element))
    _ <- Parse.runCheck(_.checkNoMixedContent)
    // TODO allow character content - or not...
    result <- parser
    _ <- Parse.runCheck(_.checkNoLeftovers)
    _ <- Parse.modify(_.pop)
  } yield result
}
