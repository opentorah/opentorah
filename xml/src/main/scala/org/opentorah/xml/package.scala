package org.opentorah

import zio.{IO, ZIO}

package object xml {

  type Error = String

  type Result = IO[Error, Unit]

  val ok: Result = IO.succeed(())

  type Parser[+A] = ZIO[Context, Error, A]
}
