package org.opentorah.xml

import org.opentorah.util.Effects
import java.net.URL
import zio.{Has, ZIO}

final /* TODO private[xml]? */ class Context {

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

  private def currentBaseUrl: Option[URL] =
    stack.flatMap(_.from).head.url

  private def currentFromUrl: FromUrl = new FromUrl(
    url = currentBaseUrl.get,
    inline = stack.head.from.isEmpty
  )

  private def currentToString: String =
    stack.headOption.map(_.toString).getOrElse("")
}

private[xml] object Context {
  val isEmpty: Parser[Boolean] =
    ZIO.access[Has[Context]](_.get.isEmpty)

  val elementName: Parser[String] =
    lift(_.name)

  def nextElement(p: Xml.Element => Boolean): Parser[Option[Xml.Element]] =
    liftContentModifier(_.takeNextElement(p))

  def takeAttribute(attribute: Attribute[_]): Parser[Option[String]] =
    liftCurrentModifier(_.takeAttribute(attribute))

  val takeAllAttributes: Parser[Seq[Attribute.Value[String]]] =
    liftCurrentModifier(_.takeAllAttributes)

  val takeCharacters: Parser[Option[String]] =
    liftContentModifier(_.takeCharacters)

  val allNodes: Parser[Xml.Nodes] =
    liftContentModifier(_.takeAllNodes)

  private def lift[A](f: Current => A): Parser[A] =
    ZIO.access[Has[Context]](context => liftCurrentToContext(f)(context.get))

  private def liftCurrentModifier[A]: (Current => Current.Next[A]) => Parser[A] = (f: Current => Current.Next[A]) => for {
    result <- ZIO.accessM[Has[Context]](context => liftCurrentToContext(f)(context.get))
    _ <- ZIO.access[Has[Context]](_.get.replaceCurrent(result._1))
  } yield result._2

  private def liftContentModifier[A]: (Content => Content.Next[A]) => Parser[A] =
    liftCurrentModifier[A] compose liftContentModifierToCurrentModifier[A]

  private def liftCurrentToContext[A](f: Current => A): Context => A =
    (context: Context) => f(context.current)

  private def liftContentModifierToCurrentModifier[A](f: Content => Content.Next[A]): Current => Current.Next[A] =
    (current: Current) =>
      f(current.content).map { case (content, result) => (current.copy(content = content), result) }

  def currentBaseUrl: Parser[Option[URL]] =
    ZIO.access[Has[Context]](_.get.currentBaseUrl)

  def currentFromUrl: Parser[FromUrl] =
    ZIO.access[Has[Context]](_.get.currentFromUrl)

  def nested[A](
    from: Option[From],
    nextElement: Xml.Element,
    contentType: ContentType,
    parser: Parser[A]
  ): Parser[A] = for  {
    content <- Content.open(nextElement.child, contentType)
    newCurrent = Current(
      from,
      name = nextElement.label,
      attributes = Xml.getAttributes(nextElement),
      content
    )
    result <- ZIO.access[Has[Context]](_.get.push(newCurrent)).bracket[Has[Context], Effects.Error, A](
      release = (_: Unit) => ZIO.access[Has[Context]](_.get.pop()),
      use = (_: Unit) => addErrorTrace(for { result <- parser; _ <- checkNoLeftovers } yield result)
    )
  }  yield result

  private def addErrorTrace[A](parser: Parser[A]): Parser[A] = parser.flatMapError(error => for {
    contextStr <- ZIO.access[Has[Context]](_.get.currentToString)
  } yield error + "\n" + contextStr)

  def checkNoLeftovers: Parser[Unit] = for {
    isEmpty <- ZIO.access[Has[Context]](_.get.isEmpty)
    _ <- if (isEmpty) Effects.ok else ZIO.accessM[Has[Context]](context => liftCurrentToContext(_.checkNoLeftovers)(context.get))
  } yield ()
}
