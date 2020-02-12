package org.digitaljudaica.xml

import cats.implicits._

object Check {

  def apply(condition: Boolean, message: => String): Parser[Unit] = if (condition) Parser.pure(()) else for {
    contextStr <- Parser.inspect(_.toString)
    _ <- Parser.error[Unit](message + "\n" + contextStr)
  } yield ()

  private[xml] def apply(f: Context => Parser[Unit]): Parser[Unit] = for {
    toRun <- Parser.inspect(f)
    _ <- toRun
  } yield ()

  private[xml] def required[A](what: String, parser: Parser[Option[A]]): Parser[A] = for {
    result <- parser
    _ <- Check(result.isDefined, s"Required $what is missing")
  } yield result.get
}
