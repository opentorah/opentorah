package org.opentorah.xml

import java.net.URL
import zio.ZIO
import scala.xml.{Elem, Node}

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

  private def isEmpty: Boolean =
    stack.isEmpty

  private def currentFromUrl: Option[URL] =
    stack.flatMap(_.from).head.url

  private def currentFromUrls: FromUrls = {
    val froms: Seq[Option[From]] = stack.map(_.from)
    val baseUrl: URL = froms.flatten.head.url.get
    if (froms.head.isEmpty) FromUrls.Nested(baseUrl) else FromUrls.Top(baseUrl, redirectedFrom = {
      val (redirectedTo: Seq[Option[From]], tail: Seq[Option[From]]) = froms.span(_.get.isRedirect)
      if (redirectedTo.isEmpty) Seq.empty else (redirectedTo.tail :+ tail.head).map(_.get.url.get)
    })
  }

  private def currentToString: String =
    stack.headOption.map(_.toString).getOrElse("")
}

private[xml] object Context {
  val isEmpty: Parser[Boolean] =
    ZIO.access[Context](_.isEmpty)

  val elementName: Parser[String] =
    lift(_.name)

  def nextElement(p: Elem => Boolean): Parser[Option[Elem]] =
    liftContentModifier(_.takeNextElement(p))

  def takeAttribute(attribute: Attribute[_]): Parser[Option[String]] =
    liftCurrentModifier(_.takeAttribute(attribute))

  val takeAllAttributes: Parser[Seq[Attribute.Value[String]]] =
    liftCurrentModifier(_.takeAllAttributes)

  val takeCharacters: Parser[Option[String]] =
    liftContentModifier(_.takeCharacters)

  val allNodes: Parser[Seq[Node]] =
    liftContentModifier(_.takeAllNodes)

  private def lift[A](f: Current => A): Parser[A] =
    ZIO.access[Context](liftCurrentToContext(f))

  private def liftCurrentModifier[A]: (Current => Current.Next[A]) => Parser[A] = (f: Current => Current.Next[A]) => for {
    result <- ZIO.accessM[Context](liftCurrentToContext(f))
    _ <- ZIO.access[Context](_.replaceCurrent(result._1))
  } yield result._2

  private def liftContentModifier[A]: (Content => Content.Next[A]) => Parser[A] =
    liftCurrentModifier[A] compose liftContentModifierToCurrentModifier[A]

  private def liftCurrentToContext[A](f: Current => A): Context => A =
    (context: Context) => f(context.current)

  private def liftContentModifierToCurrentModifier[A](f: Content => Content.Next[A]): Current => Current.Next[A] =
    (current: Current) =>
      f(current.content).map { case (content, result) => (current.copy(content = content), result) }

  def currentFromUrl: Parser[Option[URL]] =
    ZIO.access[Context](_.currentFromUrl)

  def currentFromUrls: Parser[FromUrls] =
    ZIO.access[Context](_.currentFromUrls)

  def nested[A](newCurrent: Current, parser: Parser[A]): Parser[A] =
    ZIO.access[Context](_.push(newCurrent)).bracket[Context, Error, A](
      release = (_: Unit) => ZIO.access[Context](_.pop()),
      use = (_: Unit) => addErrorTrace(for { result <- parser; _ <- checkNoLeftovers } yield result)
    )

  private def addErrorTrace[A](parser: Parser[A]): Parser[A] = parser.flatMapError(error => for {
    contextStr <- ZIO.access[Context](_.currentToString)
  } yield error + "\n" + contextStr)

  def checkNoLeftovers: Parser[Unit] = for {
    isEmpty <- ZIO.access[Context](_.isEmpty)
    _ <- if (isEmpty) ok else ZIO.accessM(liftCurrentToContext(_.checkNoLeftovers))
  } yield ()
}
