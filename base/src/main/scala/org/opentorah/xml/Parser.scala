package org.opentorah.xml

import org.opentorah.util.Effects
import zio.{Has, Task, ZIO, ZLayer}

type Parser[+A] = ZIO[Has[Parsing], Effects.Error, A]

object Parser {
  def unsafeRun[A](parser: Parser[A]): A = Effects.unsafeRun(toTask(parser))

  def toTask[A](parser: Parser[A]): Task[A] = Effects.error2throwable(addErrorTrace(
    for
      result: A <- parser
      _ <- Parsing.checkEmpty
    yield result
  )).provideLayer(ZLayer.succeed(Parsing.empty))

  // TODO report error better: effect.tapCause(cause => console.putStrLn(cause.prettyPrint))?
  private def addErrorTrace[A](parser: Parser[A]): Parser[A] =
    parser.flatMapError(Parsing.addErrorTrace)
}
