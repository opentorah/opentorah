package org.opentorah.util

import zio.{Runtime, Task, ZIO}

object Effects:

  class Error(message: String, cause: Throwable) extends Throwable(message, cause):
    def this(message: String) = this(message, null)
    def this(cause: Throwable) = this(null, cause)

  private def throwable2error[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, Error, A] = zio.mapError {
    case error: Error => error
    case error => new Error(error)
  }

  type IO[A] = zio.IO[Error, A]

  val ok: IO[Unit] = ZIO.succeed(())

  def fail(message: String): IO[Nothing] = ZIO.fail(new Error(message))

  def check(condition: Boolean, message: => String): IO[Unit] =
    if condition then ok else fail(message)

  // Note: In the past, when running with 1 cpu (on Cloud Run or in local Docker),
  // I had to use blockingRuntime to avoid deadlocks with nested unsafeRun() calls;
  // this seems to be no longer necessary.
  // TODO how am I supposed to do unsafeRun?
  // The following is from https://www.reddit.com/r/scala/comments/vjrvux/zio_20_released/
  // from ZIO Test documentation:
  //Unsafe.unsafe { implicit unsafe =>
  //  Runtime.default.unsafe.run(
  //    Random.nextIntBounded(10)
  //  ).getOrThrowFiberFailure()
  //}
  implicit class RuntimeExtension(thiz: Runtime[Any]) extends AnyVal:
    def unsafeRun[E, A](prog: zio.IO[E, A]): A =
      zio.Unsafe.unsafe(implicit _ => thiz.unsafe.run(prog).getOrThrowFiberFailure())

  def unsafeRun[E <: Throwable, A](io: => zio.IO[E, A]): A = Runtime.default.unsafeRun(io)

  def effect[A](f: => A): IO[A] = throwable2error(ZIO.attemptBlocking(f))

  def collectAll[R, A](zios: Seq[ZIO[R, Error, A]]): ZIO[R, Error, Seq[A]] = for
    runs: Seq[Either[Error, A]] <- ZIO.foreach(zios)(_.either)
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
