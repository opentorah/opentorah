package org.opentorah.xml

import org.opentorah.util.Effects
import zio.{Task, ZIO, ZLayer}

type Parser[+A] = ZIO[Parsing, Effects.Error, A]

object Parser:
  def unsafeRun[A](parser: Parser[A]): A = Effects.unsafeRun(toTask(parser))

  def toTask[A](parser: Parser[A]): Task[A] = (
    for
      result: A <- parser
      _ <- Parsing.checkEmpty
    yield result
  )
    .flatMapError(Parsing.addErrorTrace)
    .provideLayer(ZLayer.succeed(Parsing.empty))
