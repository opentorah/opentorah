package org.opentorah.archive.collector

import org.opentorah.store.{Store, WithPath}
import org.opentorah.xml.PaigesPrettyPrinter

object Util {

  def teiPrettyPrinter: PaigesPrettyPrinter = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline", "item"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )

  def htmlPrettyPrinter: PaigesPrettyPrinter = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
  )

  def getCollections(store: Store): Seq[WithPath[Collection]] =
    store.withPath[Collection](values = {
      case collection: Collection => Seq(collection)
      case _ => Seq.empty
    })
}
