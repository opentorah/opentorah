package org.digitaljudaica.xml

import zio.{DefaultRuntime, IO}

// TODO move all this stuff into non-Parser-related class (or util package?) for reuse in 'collector'...
object Parser {

  def succeed[A](value: A): Parser[A] = IO.succeed(value)

  val ok: IO[Error, Unit] = IO.succeed(())

  private[xml] def effect[A](f: => A): IO[Error, A] = IO(f).mapError(_.getMessage)

  def fail[A](value: Error): IO[Error, A] = IO.fail(value)

  def check(condition: Boolean, message: => String): IO[Error, Unit] =
    if (condition) ok else fail(message)

  private[xml] def required[A](what: String, parser: Parser[Option[A]]): Parser[A] = for {
    result <- parser
    _ <- check(result.isDefined, s"Required $what is missing")
  } yield result.get

  final def run[A](toRun: IO[Error, A]): A =
    new DefaultRuntime {}.unsafeRun(toRun.mapError(error => throw new IllegalArgumentException(error)))
}
