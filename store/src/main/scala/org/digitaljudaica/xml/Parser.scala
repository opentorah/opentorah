package org.digitaljudaica.xml

import zio.ZIO

object Parser {

  def pure[A](value: A): Parser[A] = ZIO.succeed(value)

  def lift[A](value: ErrorOr[A]): Parser[A] = ZIO.fromEither(value)

  def error[A](value: Error): Parser[A] = ZIO.fail(value)

  def check(condition: Boolean, message: => String): Parser[Unit] =
    if (condition) pure(()) else error[Unit](message)

  private[xml] def required[A](what: String, parser: Parser[Option[A]]): Parser[A] = for {
    result <- parser
    _ <- check(result.isDefined, s"Required $what is missing")
  } yield result.get

  // TODO remove
  def toErrorOr[A](f: => A): ErrorOr[A] =
    try { Right(f) } catch { case e: Exception => Left(e.getMessage) }

  def toParser[A](f: => A): Parser[A] = ZIO(f).mapError(_.getMessage)
}
