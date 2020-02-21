package org.digitaljudaica.xml

import java.net.URL
import zio.{IO, ZIO}
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

  def lift[A](f: Current => A): Parser[A] =
    ZIO.access[Context](liftCurrentToContext(f))

  private def liftCurrentToContext[A](f: Current => A): Context => A = (context: Context) => f(context.current)

  def liftCurrentModifier[A]: Current.Modifier[A] => Parser[A] = (f: Current.Modifier[A]) => for {
    result <- ZIO.accessM[Context](liftCurrentToContext(f))
    _ <- ZIO.access[Context](_.replaceCurrent(result._1))
  } yield result._2

  def liftContentModifier[A]: Content.Modifier[A] => Parser[A] =
    Context.liftCurrentModifier[A] compose liftContentModifierToCurrentModifier[A]

  private def liftContentModifierToCurrentModifier[A](f: Content.Modifier[A]): Current.Modifier[A] = (current: Current) =>
    f(current.content).map { case (content, result) => (current.copy(content = content), result) }

  def include[A](url: String, contentType: ContentType, parser: Parser[A]): Parser[A] = for {
    _ <- checkNoLeftovers
    currentFrom <- ZIO.access[Context](_.currentFrom)
    from <- Parser.effect(From.url(currentFrom.url.fold(new URL(url))(new URL(_, url))))
    result <- nested(from, contentType, parser)
  } yield result

  def nested[A](from: From, contentType: ContentType, parser: Parser[A]): Parser[A] = for {
    elem <- from.load
    result <- nested(Some(from), elem, contentType, parser)
  } yield result

  def nested[A](
    from: Option[From],
    elem: Elem,
    contentType: ContentType,
    parser: Parser[A]
  ): Parser[A] = for {
    newCurrent <- Current.open(from, elem, contentType)
    _ <- ZIO.access[Context](_.push(newCurrent))
    result <- parser
    _ <- checkNoLeftovers
    _ <- ZIO.access[Context](_.pop())
  } yield result

  private def checkNoLeftovers: Parser[Unit] =
    ZIO.accessM(liftCurrentToContext(Current.checkNoLeftovers))

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
}
