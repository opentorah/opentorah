package org.opentorah.archive.collector

import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.{LanguageSpec, Name, Names}
import org.opentorah.store.{Selector, Store}
import org.opentorah.tei.Tei

final class Document(
  inheritedSelectors: Seq[Selector],
  url: URL,
  val tei: Tei,
  val name: String,
  val translations: Map[String, Tei]
) extends Store(inheritedSelectors, url) {

  override def toString: String = name

  override def names: Names = new Names(Seq(new Name(name, LanguageSpec.empty)))

  override def references: Seq[EntityReference] = tei.references
}
