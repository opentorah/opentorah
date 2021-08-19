package org.opentorah.xml

import org.opentorah.util.Effects
import zio.{Task, ZLayer}

// TODO dissolve?
object Parser {

  def unsafeRun[A](parser: Parser[A]): A = Effects.unsafeRun(toTask(parser))

  // TODO report error better: effect.tapCause(cause => console.putStrLn(cause.prettyPrint))
  // see Context.nested()
  def toTask[A](parser: Parser[A]): Task[A] = Effects.error2throwable(
    for {
      result <- parser
      _ <- Context.checkEmpty
    } yield result
  ).provideLayer(ZLayer.succeed(new Context))
}
