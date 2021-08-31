package org.opentorah.store

import org.opentorah.metadata.Named

trait Store extends Named

object Store {
  type Path = Seq[Store]

  trait NonTerminal extends Store with Stores

  trait Terminal extends Store
}
