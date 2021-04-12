package org.opentorah.xml

import org.opentorah.util.Effects
import zio.{Task, ZLayer}

object Parser {

  // TODO rename unsafeRun
  def run[A](parser: Parser[A]): A = Effects.unsafeRun(toTask(parser))

  // TODO report error better: effect.tapCause(cause => console.putStrLn(cause.prettyPrint))
  // see Context.nested()
  def toTask[A](parser: Parser[A]): Task[A] = Effects.error2throwable(
    for {
      result <- parser
      isEmpty <- Context.isEmpty
      _ <- if (isEmpty) Effects.ok else throw new IllegalStateException(s"Non-empty context $this!")
    } yield result
  ).provideLayer(ZLayer.succeed(new Context))
}
