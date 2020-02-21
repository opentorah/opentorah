package org.digitaljudaica.xml

import java.net.URL
import zio.{DefaultRuntime, IO, ZIO}
import scala.xml.Elem

final private[xml] class Context {

  private var stack: List[Current] = List.empty

  override def toString: String =
    stack.mkString("\n")

  private def current: Current =
    stack.head

  private def replaceCurrent[A](newCurrent: Current): Unit =
    stack = newCurrent :: stack.tail

  private def push(element: Current): Unit =
    stack = element :: stack

  private def pop(): Unit =
    stack = stack.tail

  private def checkIsEmpty(): Unit =
    if (stack.nonEmpty) throw new IllegalStateException(s"Non-empty context $this!")

  private def currentFrom: From =
    stack.flatMap(_.from).head
}

private[xml] object Context {

  def inspectCurrent[A](f: Current => A): Parser[A] =
    ZIO.access[Context](context => f(context.current))

  def lift[A]: Current.Modifier[A] => Parser[A] = (f: Current.Modifier[A]) => for {
    toRun <- inspectCurrent[ErrorOr[(Current, A)]](f) // TODO use accessM...
    result <- Parser.lift(toRun)
    _ <- ZIO.access[Context](_.replaceCurrent(result._1))
  } yield result._2

  def include[A](url: String, parser: Parser[A]): Parser[A] = for {
    _ <- checkNoLeftovers
    name <- Xml.name
    currentFrom <- ZIO.access[Context](_.currentFrom)
    from <- Parser.toParser(From.url(currentFrom.url.fold(new URL(url))(new URL(_, url))))
    result <- nested(from, ContentType.Elements, Xml.withName(name, parser)) // TODO make ContentType changeable?
  } yield result

  def nested[A](from: From, contentType: ContentType, parser: Parser[A]): Parser[A] = for {
    elem <- from.load
    result <- nested(Some(from), elem, parser, contentType)
  } yield result

  def nested[A](
    from: Option[From],
    elem: Elem,
    parser: Parser[A],
    contentType: ContentType
  ): Parser[A] = for {
    newCurrent <- Parser.lift(Current.open(from, elem, contentType))
    _ <- ZIO.access[Context](_.push(newCurrent))
    result <- parser
    _ <- checkNoLeftovers
    _ <- ZIO.access[Context](_.pop())
  } yield result

  private def checkNoLeftovers: Parser[Unit] = for {
    checkNoLeftovers <- inspectCurrent(Current.checkNoLeftovers) // TODO use accessM...
    _ <- ZIO.fromEither(checkNoLeftovers)
  } yield ()

  def runnable[A](parser: Parser[A]): IO[Error, A] = {
    val toRun: Parser[A] = for {
      result <- parser
      _ <- ZIO.access[Context](_.checkIsEmpty())
    } yield result

    val result: Parser[A] = toRun.flatMapError(error => for {
      contextStr <- ZIO.access[Context](_.toString)
    } yield error + "\n" + contextStr)

    result.provide(new Context)
  }

  final def run[A](toRun: IO[Error, A]): A =
    new DefaultRuntime {}.unsafeRun(toRun.mapError(error => throw new IllegalArgumentException(error)))
}
