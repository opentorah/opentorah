package org.opentorah.store

import org.opentorah.metadata.Named

trait Store extends Named with FindByName

object Store {
  type Path = Seq[Store]
}
