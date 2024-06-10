package org.opentorah.xml

import org.opentorah.util.Effects
import zio.{Task, ZIO, ZLayer}

type Parser[+A] = ZIO[ParserState, Effects.Error, A]

object Parser:
  def unsafeRun[A](parser: Parser[A]): A = Effects.unsafeRun(toTask(parser))

  def toTask[A](parser: Parser[A]): Task[A] = (
    for
      result: A <- parser
      _ <- ParserState.checkEmpty
    yield result
  )
    .flatMapError(ParserState.addErrorTrace)
    .provideLayer(ZLayer.succeed(ParserState.empty))
