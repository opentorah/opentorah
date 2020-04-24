package org.opentorah

import zio.ZIO

package object xml {

  type Error = String

  type Parser[+A] = ZIO[Context, Error, A]
}
