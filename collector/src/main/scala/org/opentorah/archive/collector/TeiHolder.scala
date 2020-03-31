package org.opentorah.archive.collector

import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.{Name, Names}
import org.opentorah.store.{Selector, Store}
import org.opentorah.tei.Tei

final class TeiHolder(
  inheritedSelectors: Seq[Selector],
  fromUrl: URL,
  val name: String,
  val tei: Tei
) extends Store(inheritedSelectors, Some(fromUrl), fromUrl) {

  override def names: Names = new Names(Seq(Name(name)))

  override def references: Seq[EntityReference] = tei.references
}
