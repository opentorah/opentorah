package org.digitaljudaica.xml

import java.net.URL
import cats.implicits._
import scala.xml.Elem

final class Context private(private val stack: List[Current]) {

  override def toString: String =
    stack.mkString("\n")

  private def current: Current =
    stack.head

  private def replaceCurrent[A](newCurrent: Current): Context =
    new Context(stack = newCurrent :: stack.tail)

  private def push(element: Current): Context =
    new Context(element :: stack)

  private def pop: Context =
    new Context(stack.tail)

  private def checkIsEmpty(): Unit =
    if (stack.nonEmpty) throw new IllegalStateException(s"Non-empty context $this!")

  private def currentFrom: From =
    stack.flatMap(_.from).head
}

private[xml] object Context {

  private val currentFrom: Parser[From] =
    Parser.inspect(_.currentFrom)

  def inspectCurrent[A](f: Current => A): Parser[A] =
    Parser.inspect(context => f(context.current))

  def replaceCurrent[A](f: Current => ErrorOr[(Current, A)]): Parser[A] = for {
    toRun <- Parser.inspect(context => f(context.current))
    result <- Parser.lift(toRun)
    _ <- Parser.modify(_.replaceCurrent(result._1))
  } yield result._2

  def include[A](url: String, parser: Parser[A]): Parser[A] = for {
    _ <- checkNoLeftovers
    name <- Xml.name
    currentFrom <- currentFrom
    from <- Parser.toParser(From.url(currentFrom.url.fold(new URL(url))(new URL(_, url))))
    result <- nested(from, ContentType.Elements, Xml.withName(name, parser)) // TODO make changeable?
  } yield result

  def nested[A](from: From, contentType: ContentType, parser: Parser[A]): Parser[A] = from.load.fold(
    error => Parser.error(error),
    elem => nested(Some(from), elem, parser, contentType)
  )

  def nested[A](
    from: Option[From],
    elem: Elem,
    parser: Parser[A],
    contentType: ContentType
  ): Parser[A] = for {
    newCurrent <- Parser.lift(Current.open(from, elem, contentType))
    _ <- Parser.modify(_.push(newCurrent))
    result <- parser
    _ <- checkNoLeftovers
    _ <- Parser.modify(_.pop)
  } yield result

  private def checkNoLeftovers: Parser[Unit] = for {
    checkNoLeftovers <- inspectCurrent(_.checkNoLeftovers)
    _ <- Parser.lift(checkNoLeftovers)
  } yield ()

  // TODO capture Context in the error message!
  def parse[A](parser: Parser[A]): ErrorOr[A] = {
    val toRun = for {
      result <- parser
      _ <- Parser.inspect(_.checkIsEmpty())
    } yield result

    // TODO I need to obtain th final Context in case of error, and tack its toString() to the message...
    // If there i sno way to run the parser and obtain the final state, I'll need to capture the Context
    // at the time of raising the error, byy turning Parser.error() into a Parser - and use it in From.load(),
    // convertions etc. ...
    toRun.runA(new Context(List.empty))
  }
}
