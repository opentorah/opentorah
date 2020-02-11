package org.digitaljudaica.xml

import cats.implicits._

object Check {

  def apply(condition: Boolean, message: => String): Parser[Unit] = if (condition) pure(()) else for {
    contextStr <- inspect(_.toString)
    _ <- error(message + "\n" + contextStr)
  } yield ()

  private[xml] def apply(f: Context => Parser[Unit]): Parser[Unit] = for {
    toRun <- inspect(f)
    _ <- toRun
  } yield ()

  def error(value: Error): Parser[Unit] = lift[Unit](Left(value))

  private[xml] def required[A](what: String, parser: Parser[Option[A]]): Parser[A] = for {
    result <- parser
    _ <- Check(result.isDefined, s"Required $what is missing")
  } yield result.get
}
