package org.opentorah

import org.opentorah.util.Effects
import zio.{Has, ZIO}

package object xml {

  type Parser[+A] = ZIO[Has[Context], Effects.Error, A]
}
