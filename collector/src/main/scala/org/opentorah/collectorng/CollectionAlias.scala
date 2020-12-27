package org.opentorah.collectorng

import org.opentorah.metadata.Names

final class CollectionAlias(val collection: Collection) extends Store {

  def alias: String = collection.alias.get

  override val names: Names = Names(alias)

  override def findByName(name: String): Option[Store] = collection.findByName(name)
}
