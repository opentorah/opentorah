package org.opentorah.archive.collector

import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.{LanguageSpec, Name, Names}
import org.opentorah.store.{Path, Store}
import org.opentorah.tei.Tei

final class Document(
  parent: Option[Store],
  url: URL,
  val tei: Tei,
  val name: String,
  val translations: Map[String, Tei]
) extends Store(parent, url) {

  override def toString: String = name

  override def names: Names = new Names(Seq(new Name(name, LanguageSpec.empty)))

  override def references(at: Path): Seq[EntityReference] = tei.references.map(_.at(at))
}
