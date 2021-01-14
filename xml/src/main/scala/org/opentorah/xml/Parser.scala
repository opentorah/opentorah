package org.opentorah.xml

import zio.{IO, Runtime, ZIO}

object Parser {

  private[xml] def required[T](parser: Parser[Option[T]], what: AnyRef): Parser[T] = for {
    result <- parser
    _ <- check(result.isDefined, s"Required $what is missing")
  } yield result.get

  def collectAll[A](parsers: Seq[Parser[A]]): Parser[Seq[A]] = for {
    runs <- ZIO.foreach(parsers)(_.either)
    errors: Seq[Error] = runs.flatMap(_.left.toOption)
    results: Seq[A] = runs.flatMap(_.toOption)
    results <- if (errors.nonEmpty) IO.fail(errors.mkString("Errors:\n  ", "\n  ", "\n.")) else IO.succeed(results)
  } yield results

  def mapValues[A, B, C](map: Map[A, B])(f: B => Parser[C]): Parser[Map[A, C]] =
    collectAll(map.toSeq.map { case (a, b) => f(b).map(a -> _) }).map(_.toMap)

  private[xml] def effect[A](f: => A): IO[Error, A] = IO(f).mapError(_.getMessage)

  def check(condition: Boolean, message: => String): Result =
    if (condition) ok else IO.fail(message)

  def parseDo[A](parser: Parser[A]): A =
    run(runnable(parser))

  private[xml] def runnable[A](parser: Parser[A]): IO[Error, A] = {
    val result: Parser[A] = for {
      result <- parser
      isEmpty <- Context.isEmpty
      _ <- if (isEmpty) ok else throw new IllegalStateException(s"Non-empty context $this!")
    } yield result

    result.provide(new Context)
  }

  // TODO report error better: effect.tapCause(cause => console.putStrLn(cause.prettyPrint))
  private[xml] final def run[A](toRun: IO[Error, A]): A =
    Runtime.default.unsafeRun(toRun.mapError(error => throw new IllegalArgumentException(error)))

  final def load(from: From): Xml.Element = Parser.run(from.load)
}
