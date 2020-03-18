package org.opentorah.store

import java.net.URL
import org.opentorah.reference.{Named, NamedsList, Reference}
import org.opentorah.xml.Parser

// TODO turn into Store with two Bys
final class Nameds(element: NamedsElement, selectorByName: String => Selector, url: URL) {
  def selector: Selector.Nullary = selectorByName(element.selector).asNullary

  val by: Nameds.NamedsBy = new Nameds.NamedsBy {
    override def selector: Selector.Named = selectorByName(element.by).asNamed

    override val stores: Seq[Named] = Parser.parseDo(Named.parseAll(url, element.directory, element.list))
  }

  def lists: Seq[NamedsList] = element.lists.map(_.take(by.stores))

  def findByRef(ref: String): Option[Named] = by.stores.find(_.id.get == ref)

  def references(at: Path): Seq[Reference] = by.references(at :+ selector.bind)
}

object Nameds {
  trait NamedsBy extends By {
    override def stores: Seq[Named]
  }
}
