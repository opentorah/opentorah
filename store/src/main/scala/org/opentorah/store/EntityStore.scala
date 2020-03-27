package org.opentorah.store

import java.net.URL
import org.opentorah.entity.{Entity, EntityReference}
import org.opentorah.metadata.{Language, Name, Names}

final class EntityStore(
  inheritedSelectors: Seq[Selector],
  url: URL,
  val entity: Entity
) extends Store(inheritedSelectors, url) {

  override def names: Names =
    new Names(Seq(Name(entity.name, Language.Russian)))

  override def references: Seq[EntityReference] = entity.references
}
