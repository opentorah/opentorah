package org.digitaljudaica.store

import org.digitaljudaica.metadata.Names
import org.digitaljudaica.xml.Parser

final case class Selector(names: Names)

object Selector {

  val parser: Parser[Selector] = for {
    names <- Names.parser // TODO handle default name
  } yield Selector(names)
}
