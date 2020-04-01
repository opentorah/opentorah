package org.opentorah.xml

import zio.{IO, Runtime, ZIO}

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

  def parseDo[A](parser: Parser[A]): A =
    run(runnable(parser))

  private[xml] def runnable[A](parser: Parser[A]): IO[Error, A] = {
    val result: Parser[A] = for {
      result <- parser
      isEmpty <- Context.isEmpty
      _ <- if (isEmpty) IO.succeed(()) else throw new IllegalStateException(s"Non-empty context $this!")
    } yield result

    result.provide(new Context)
  }

  private[xml] final def run[A](toRun: IO[Error, A]): A =
    Runtime.default.unsafeRun(toRun.mapError(error => throw new IllegalArgumentException(error)))
}
