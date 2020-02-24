package org.digitaljudaica.xml

import zio.{DefaultRuntime, IO, ZIO}

object Parser {

  private[xml] def required[A](what: String, parser: Parser[Option[A]]): Parser[A] = for {
    result <- parser
    _ <- check(result.isDefined, s"Required $what is missing")
  } yield result.get

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

    addErrorTrace(result).provide(new Context)
  }

  private def addErrorTrace[A](parser: Parser[A]): Parser[A] = parser.flatMapError(error => for {
    contextStr <- ZIO.access[Context](_.toString)
  } yield error + "\n" + contextStr)

  // TODO move into non-Parser-related class (or util package?) for reuse (in 'collector' etc.)

  private[xml] def effect[A](f: => A): IO[Error, A] = IO(f).mapError(_.getMessage)

  def check(condition: Boolean, message: => String): IO[Error, Unit] =
    if (condition) IO.succeed(())
    else IO.fail(message)

  final def run[A](toRun: IO[Error, A]): A =
    new DefaultRuntime {}.unsafeRun(toRun.mapError(error => throw new IllegalArgumentException(error)))
}
