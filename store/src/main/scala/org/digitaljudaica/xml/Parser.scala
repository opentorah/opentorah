package org.digitaljudaica.xml

import zio.{DefaultRuntime, IO}

object Parser {

  def succeed[A](value: A): Parser[A] = IO.succeed(value)

  // TODO move into non-Parser-related class (or util package?) for reuse (in 'collector' etc.)
  private[xml] def effect[A](f: => A): IO[Error, A] = IO(f).mapError(_.getMessage)

  // TODO move into non-Parser-related class (or util package?) for reuse (in 'collector' etc.)
  def check(condition: Boolean, message: => String): IO[Error, Unit] =
    if (condition) IO.succeed(()) else IO.fail(message)

  private[xml] def required[A](what: String, parser: Parser[Option[A]]): Parser[A] = for {
    result <- parser
    _ <- check(result.isDefined, s"Required $what is missing")
  } yield result.get

  // TODO move into non-Parser-related class (or util package?) for reuse (in 'collector' etc.)
  final def run[A](toRun: IO[Error, A]): A =
    new DefaultRuntime {}.unsafeRun(toRun.mapError(error => throw new IllegalArgumentException(error)))
}
