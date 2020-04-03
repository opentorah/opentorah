package org.opentorah.collector

import org.opentorah.entity.EntityReference
import org.opentorah.metadata.{Name, Names}
import org.opentorah.store.{Selector, Store, Urls}
import org.opentorah.tei.Tei

final class TeiHolder(
  inheritedSelectors: Seq[Selector],
  urls: Urls,
  val name: String,
  val language: Option[String],
  val tei: Tei
) extends Store(inheritedSelectors, urls) {

  override def names: Names = new Names(Seq(Name(name)))

  override def references: Seq[EntityReference] = tei.references
}
