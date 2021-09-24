package org.opentorah.util

import zio.blocking.Blocking
import zio.{Runtime, Task, ZIO}

object Effects:

  class Error(message: String, cause: Throwable) extends Throwable(message, cause):
    def this(message: String) = this(message, null)
    def this(cause: Throwable) = this(null, cause)
  
  def error2throwable[R, A](zio: ZIO[R, Error, A]): ZIO[R, Throwable, A] =
    zio.mapError(IllegalArgumentException(_))

  private def throwable2error[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, Error, A] = zio.mapError {
    case error: Error => error
    case error => new Error(error)
  }

  type IO[A] = zio.IO[Error, A]

  val ok: IO[Unit] = ZIO.succeed(())

  def fail(message: String) = ZIO.fail(new Error(message))
  
  def check(condition: Boolean, message: => String): IO[Unit] =
    if condition then ok else fail(message)

  // Note: when running with 1 cpu (on Cloud Run or in local Docker),
  // take care to avoid deadlocks:

  // Note: using blockingRuntime to avoid deadlocks with nested unsafeRun() calls (TODO is this still needed?):
  private val blockingRuntime: Runtime[?] = Runtime.default.withExecutor(Blocking.Service.live.blockingExecutor)
  // ZIO 2:   private val blockingRuntime: Runtime[?] = Runtime.default.withExecutor(Runtime.default.platform.blockingExecutor)

  def unsafeRun[E, A](io: => zio.IO[E, A]): A =
    blockingRuntime.unsafeRun[E, A](io)

  // Note: run effects on the blocking threadpool:
  def effectTotal[A](effect: => A): Task[A] =
    Blocking.Service.live.effectBlocking(effect) // ZIO 2: ZIO.succeedBlocking(effect)

  // Note: run effects on the blocking threadpool:
  def attempt[A](f: => A): Task[A] =
    Blocking.Service.live.effectBlocking(f) // ZIO 2: ZIO.attemptBlocking(f)

  def effect[A](f: => A): IO[A] =
    throwable2error(attempt(f))

  def collectAll[R, A](zios: Seq[ZIO[R, Error, A]]): ZIO[R, Error, Seq[A]] = for
    runs <- ZIO.foreach(zios)(_.either)
    errors: Seq[Error] = runs.flatMap(_.left.toOption)
    results: Seq[A] = runs.flatMap(_.toOption)
    _ <- check(errors.isEmpty, errors.mkString("Errors:\n  ", "\n  ", "\n."))
  yield results

  def mapValues[R, A, B, C](map: Map[A, B])(f: B => ZIO[R, Error, C]): ZIO[R, Error, Map[A, C]] =
    collectAll(map.toSeq.map((a, b) => f(b).map(a -> _))).map(_.toMap)

  // TODO is this something from ZIO already?
  def required[R, A](optional: ZIO[R, Error, Option[A]], what: AnyRef): ZIO[R, Error, A] = for
    result <- optional
    _ <- check(result.isDefined, s"Required $what is missing")
  yield result.get
