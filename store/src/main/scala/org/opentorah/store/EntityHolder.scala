package org.opentorah.store

import org.opentorah.metadata.{Name, Names}
import org.opentorah.tei.Entity

final class EntityHolder(
  inheritedSelectors: Seq[Selector],
  urls: Urls,
  val entity: Entity
) extends Store(inheritedSelectors, urls) {

  override def names: Names = new Names(Seq(Name(entity.name)))
}
