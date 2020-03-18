package org.opentorah.store

import org.opentorah.reference.Reference

trait By {
  def selector: Selector.Named

  def stores: Seq[Store]

  final def references(at: Path): Seq[Reference] = stores.flatMap { store =>
    store.references(at :+ selector.bind(store.names))
  }
}
