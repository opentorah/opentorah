package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.reference.Reference

// TODO pass in selectorsFromAbove
trait Store {
  def names: Names

  def selectors: Seq[Selector]

  final def selectorByName(name: String): Selector =
    selectors.find(_.names.hasName(name)).get

  def nameds: Option[Nameds]

  def by: Option[By]

  // TODO overriden in Named
  def references(at: Path): Seq[Reference] = {
    val fromNameds: Seq[Reference] = nameds.toSeq.flatMap(_.references(at))
    val fromBy: Seq[Reference] = by.toSeq.flatMap(_.references(at))
    (fromNameds ++ fromBy).filterNot(_.name == scala.xml.Text("?")) // TODO get rid of the filter
  }
}
