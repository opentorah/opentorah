package org.opentorah.util

import zio.blocking.Blocking
import zio.{IO, Runtime, Task, UIO, ZIO}

object Effects {

  type Error = String // TODO use more specific type

  def error2throwable[R, A](zio: ZIO[R, Error, A]): ZIO[R, Throwable, A] =
    zio.mapError(new IllegalArgumentException(_))

  def throwable2error[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, Error, A] =
    zio.mapError(_.getMessage)

  val ok: IO[Error, Unit] = IO.succeed(())

  def check(condition: Boolean, message: => String): IO[Error, Unit] =
    if (condition) ok else IO.fail(message)

  // Note: when running with 1 cpu (on Cloud Run or in local Docker),
  // take care to avoid deadlocks:

  // Note: using blockingRuntime to avoid deadlocks with nested unsafeRun() calls:
  private val blockingRuntime: Runtime[_] = Runtime.default.withExecutor(Blocking.Service.live.blockingExecutor)

  def unsafeRun[E, A](io: => IO[E, A]): A =
    blockingRuntime.unsafeRun[E, A](io)

  def effectTotal[A](effect: => A): Task[A] =
    Blocking.Service.live.effectBlocking(effect)

  // Note: run effects on the blocking threadpool:
  def effect[A](f: => A): IO[Error, A] =
    throwable2error(Blocking.Service.live.effectBlocking(f))

  def collectAll[R, A](zios: Seq[ZIO[R, Error, A]]): ZIO[R, Error, Seq[A]] = for {
    runs <- ZIO.foreach(zios)(_.either)
    errors: Seq[Error] = runs.flatMap(_.left.toOption)
    results: Seq[A] = runs.flatMap(_.toOption)
    results <- if (errors.nonEmpty) IO.fail(errors.mkString("Errors:\n  ", "\n  ", "\n.")) else IO.succeed(results)
  } yield results

  // TODO move into Effects
  def mapValues[R, A, B, C](map: Map[A, B])(f: B => ZIO[R, Error, C]): ZIO[R, Error, Map[A, C]] =
    collectAll(map.toSeq.map { case (a, b) => f(b).map(a -> _) }).map(_.toMap)

  // TODO is this something from ZIO already?
  def required[R, A](optional: ZIO[R, Error, Option[A]], what: AnyRef): ZIO[R, Error, A] = for {
    result <- optional
    _ <- check(result.isDefined, s"Required $what is missing")
  } yield result.get
}
