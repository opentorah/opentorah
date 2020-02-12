package org.digitaljudaica.xml

import cats.data.StateT
import cats.implicits._

object Parser {

  def pure[A](value: A): Parser[A] = StateT.pure[ErrorOr, Context, A](value)

  def lift[A](value: ErrorOr[A]): Parser[A] = StateT.liftF[ErrorOr, Context, A](value)

  def error[A](value: Error): Parser[A] = lift(Left(value))


  private[xml] def inspect[A](f: Context => A): Parser[A] = StateT.inspect[ErrorOr, Context, A](f)

  private[xml] def set(context: Context): Parser[Unit] = StateT.set[ErrorOr, Context](context)

  private[xml] def modify(f: Context => Context): Parser[Unit] = StateT.modify[ErrorOr, Context](f)

  // This seems like some kind of a StateT.liftXXX, but I do not see it...
  private[xml] def inspectAndSet[A](f: Context => (Context, A)): Parser[A] = for {
    result <- Parser.inspect(f)
    _ <- Parser.set(result._1)
  } yield result._2

  private[xml] def eval(f: Context => Parser[Unit]): Parser[Unit] = for {
    toRun <- Parser.inspect(f)
    _ <- toRun
  } yield ()

  def check(condition: Boolean, message: => String): Parser[Unit] = if (condition) pure(()) else for {
    contextStr <- inspect(_.toString)
    _ <- error[Unit](message + "\n" + contextStr)
  } yield ()

  private[xml] def required[A](what: String, parser: Parser[Option[A]]): Parser[A] = for {
    result <- parser
    _ <- check(result.isDefined, s"Required $what is missing")
  } yield result.get

  def toErrorOr[A](f: => A): ErrorOr[A] =
    try { Right(f) } catch { case e: Exception => Left(e.getMessage) }

  def toParser[A](f: => A): Parser[A] = lift(toErrorOr(f))

  def runA[A](result: ErrorOr[A]): A = result match {
    case Right(result) => result
    case Left(error) => throw new IllegalArgumentException(error)
  }
}
