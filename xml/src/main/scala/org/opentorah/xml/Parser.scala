package org.opentorah.xml

import zio.{IO, Runtime, ZIO}
import zio.blocking.Blocking

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
    unsafeRun(toRun.mapError(error => throw new IllegalArgumentException(error)))

  // Note: when running with 1 cpu (on Cloud Run or in local Docker),
  // take care to avoid deadlocks:

  // Note: using blockingRuntime to avoid deadlocks with nested unsafeRun() calls:
  private val blockingRuntime: Runtime[_] = Runtime.default.withExecutor(Blocking.Service.live.blockingExecutor)
  // TODO this is reused from Markdown - yuck...
  def unsafeRun[E, A](zio: => IO[E, A]): A = {
    blockingRuntime.unsafeRun[E, A](zio)
  }

  // Note: run effects on the blocking threadpool:
  private[xml] def effect[A](f: => A): IO[Error, A] =
    Blocking.Service.live.effectBlocking(f).mapError(_.getMessage)
}
