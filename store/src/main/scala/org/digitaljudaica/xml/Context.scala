package org.digitaljudaica.xml

import java.net.URL
import zio.{IO, ZIO}
import scala.xml.Elem

final private[xml] class Context {

  private var stack: List[Current] = List.empty

  private def current: Current =
    stack.head

  private def replaceCurrent[A](newCurrent: Current): Unit =
    stack = newCurrent :: stack.tail

  private def push(element: Current): Unit =
    stack = element :: stack

  private def pop(): Unit =
    stack = stack.tail

  def isEmpty: Boolean =
    stack.isEmpty

  private def currentFromUrl: Option[URL] =
    stack.flatMap(_.from).head.url

  private def currentToString: String =
    stack.headOption.map(_.toString).getOrElse("")
}

private[xml] object Context {

  def lift[A](f: Current => A): Parser[A] =
    ZIO.access[Context](liftCurrentToContext(f))

  def liftCurrentModifier[A]: Current.Modifier[A] => Parser[A] = (f: Current.Modifier[A]) => for {
    result <- ZIO.accessM[Context](liftCurrentToContext(f))
    _ <- ZIO.access[Context](_.replaceCurrent(result._1))
  } yield result._2

  def liftContentModifier[A]: Content.Modifier[A] => Parser[A] =
    liftCurrentModifier[A] compose liftContentModifierToCurrentModifier[A]

  private def liftCurrentToContext[A](f: Current => A): Context => A =
    (context: Context) => f(context.current)

  private def liftContentModifierToCurrentModifier[A](f: Content.Modifier[A]): Current.Modifier[A] =
    (current: Current) =>
      f(current.content).map { case (content, result) => (current.copy(content = content), result) }

  def currentFromUrl: Parser[Option[URL]] =
    ZIO.access[Context](_.currentFromUrl)

  def nested[A](from: From, contentType: ContentType, parser: Parser[A]): Parser[A] = for {
    _ <- checkNoLeftovers
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
    result <- nested(newCurrent, parser)
  } yield result

  private def nested[A](newCurrent: Current, parser: Parser[A]): Parser[A] =
    ZIO.access[Context](_.push(newCurrent)).bracket[Context, Error, A](
      release = (_: Unit) => ZIO.access[Context](_.pop()),
      use = (_: Unit) => addErrorTrace(for {
        result <- parser
        _ <- checkNoLeftovers
      } yield result)
    )

  private def addErrorTrace[A](parser: Parser[A]): Parser[A] = parser.flatMapError(error => for {
    contextStr <- ZIO.access[Context](_.currentToString)
  } yield error + "\n" + contextStr)

  private def checkNoLeftovers: Parser[Unit] = for {
    isEmpty <- ZIO.access[Context](_.isEmpty)
    _ <- if (isEmpty) IO.succeed(()) else ZIO.accessM(liftCurrentToContext(Current.checkNoLeftovers))
  } yield ()
}
