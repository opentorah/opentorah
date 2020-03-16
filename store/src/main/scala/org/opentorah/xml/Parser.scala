package org.opentorah.xml

import java.net.URL
import zio.{IO, Runtime, ZIO}
import scala.xml.Node

object Parser {

  def collectAll[A](parsers: Seq[Parser[A]]): Parser[Seq[A]] = for {
    runs <- ZIO.collectAll(parsers.map(_.either))
    errors: Seq[Error] = runs.flatMap(_.left.toOption)
    results: Seq[A] = runs.flatMap(_.right.toOption)
    results <- if (errors.nonEmpty) IO.fail(errors.mkString("Errors:\n  ", "\n  ", "\n.")) else IO.succeed(results)
  } yield results

  private[xml] def effect[A](f: => A): IO[Error, A] = IO(f).mapError(_.getMessage)

  def check(condition: Boolean, message: => String): IO[Error, Unit] =
    if (condition) IO.succeed(())
    else IO.fail(message)

  def withInclude[A](parser: Parser[A]): Parser[A] =
    withInclude("include", ContentType.Elements, parser)

  def withInclude[A](attributeName: String, contentType: ContentType, parser: Parser[A]): Parser[A] = {
    def parsable(elementName: String): Element[A] = new Element[A](
      elementName,
      contentType,
      parser
    )

    for {
      url <- Attribute(attributeName).optional
      result <- url.fold(parser) { url => for {
        elementName <- Element.name
        currentFromUrl <- Context.currentFromUrl
        from <- Parser.effect(From.url(currentFromUrl.fold(new URL(url))(new URL(_, url))))
        result <- from.parse(parsable(elementName))
      } yield result}
    } yield result
  }

  val allNodes: Parser[Seq[Node]] =
    Context.liftContentModifier(Content.takeAllNodes)

  // TODO eliminate
  def parseDo[A](parser: Parser[A]): A =
    Parser.run(Parser.runnable(parser))

  // TODO eliminate (used only in tests)
  def parseOrError[A](parser: Parser[A]): Either[Error, A] =
    Parser.run(Parser.runnable(parser).either)

  def runnable[A](parser: Parser[A]): IO[Error, A] = {
    val result: Parser[A] = for {
      result <- parser
      isEmpty <- ZIO.access[Context](_.isEmpty)
      _ <- if (isEmpty) IO.succeed(()) else throw new IllegalStateException(s"Non-empty context $this!")
    } yield result

    result.provide(new Context)
  }

  final def run[A](toRun: IO[Error, A]): A =
    Runtime.default.unsafeRun(toRun.mapError(error => throw new IllegalArgumentException(error)))
}
