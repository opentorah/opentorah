package org.opentorah.xml

import org.opentorah.util.Effects
import zio.{Task, ZIO, ZLayer}
import java.net.URL

type Parser[+A] = ZIO[Caching & ParserState, Effects.Error, A]

object Parser:
  def getCachedByUrl[T <: AnyRef](url: URL, load: URL => Parser[T]): Parser[T] =
    ZIO.environmentWithZIO[Caching](_.get.getCachedByUrl[T](url, load))

  def getCached[T <: AnyRef](key: AnyRef, load: => Parser[T]): Parser[T] =
    ZIO.environmentWithZIO[Caching](_.get.getCached[T](key, load))

  def unsafeRun[A](parser: Parser[A], caching: Caching = Caching.Zero): A =
    Effects.unsafeRun(toTask(parser, caching))

  def toTask[A](parser: Parser[A], caching: Caching = Caching.Zero): Task[A] = (
    for
      result: A <- parser
      _ <- ParserState.access(_.checkEmpty())
    yield result
  )
    .flatMapError((error: Effects.Error) => ZIO.service[ParserState].map(_.addErrorTrace(error)))
    .provideSomeLayer[ParserState](ZLayer.succeed(caching))
    .provideLayer(ZLayer.succeed(ParserState.empty))
