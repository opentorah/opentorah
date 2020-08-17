package org.opentorah.store

import org.opentorah.metadata.Names

abstract class StoreNg extends ComponentNg("store") {

  override type Instance <: StoreNg.Base
}

object StoreNg {

  abstract class Base(
    urls: Urls,
    inheritedSelectors: Seq[Selector],
    val names: Names
  ) extends ComponentNg.Base(urls, inheritedSelectors) {
    override def companion: StoreNg
  }
}
