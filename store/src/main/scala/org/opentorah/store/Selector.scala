package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.Parser

final case class Selector(names: Names)

object Selector {

  val parser: Parser[Selector] = for {
    names <- Names.parser // TODO handle default name
  } yield Selector(names)
}
