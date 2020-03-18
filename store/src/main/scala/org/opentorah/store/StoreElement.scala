package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.Element

final case class StoreElement(
  names: Names,
  selectors: Seq[Selector],
  by: Option[ByElement],
  nameds: Option[NamedsElement]
)

object StoreElement extends Element[StoreElement](elementName = "store", parser = for {
  names <- Names.withDefaultNameParser
  selectors <- Selector.all
  by <- ByElement.optional
  nameds <- NamedsElement.optional
} yield StoreElement(
  names,
  selectors,
  by,
  nameds
))
