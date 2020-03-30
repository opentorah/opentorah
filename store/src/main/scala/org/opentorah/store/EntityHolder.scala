package org.opentorah.store

import java.net.URL
import org.opentorah.entity.{Entity, EntityReference}
import org.opentorah.metadata.{Name, Names}

final class EntityHolder(
  inheritedSelectors: Seq[Selector],
  fromUrl: URL,
  val entity: Entity
) extends Store(inheritedSelectors, Some(fromUrl), fromUrl) {

  override def names: Names = new Names(Seq(Name(entity.name)))

  override def references: Seq[EntityReference] = entity.references
}
