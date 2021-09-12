package org.opentorah.xml

import org.opentorah.util.Effects
import org.slf4j.{Logger, LoggerFactory}
import zio.{Has, Task, ZIO, ZLayer}

type Parser[+A] = ZIO[Has[Context], Effects.Error, A]

val xmlLogger: Logger = LoggerFactory.getLogger("org.opentorah.xml")

// TODO dissolve?
object Parser:

  def unsafeRun[A](parser: Parser[A]): A = Effects.unsafeRun(toTask(parser))

  // TODO report error better: effect.tapCause(cause => console.putStrLn(cause.prettyPrint))
  // see Context.nested()
  def toTask[A](parser: Parser[A]): Task[A] = Effects.error2throwable(
    for
      result: A <- parser
      _ <- Context.checkEmpty
    yield result
  ).provideLayer(ZLayer.succeed(new Context))
