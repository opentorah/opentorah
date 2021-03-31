package org.opentorah

import org.opentorah.util.Effects
import org.slf4j.{Logger, LoggerFactory}
import zio.{Has, ZIO}

package object xml {

  type Parser[+A] = ZIO[Has[Context], Effects.Error, A]

  val xmlLogger: Logger = LoggerFactory.getLogger("org.opentorah.xml")
}
