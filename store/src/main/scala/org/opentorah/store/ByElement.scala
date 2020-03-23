package org.opentorah.store

import org.opentorah.xml.{Attribute, Element, Parser}

final case class ByElement(
  selector: String,
  directory: Option[String],
  list: Option[String],
  stores: Seq[StoreElement]
)

object ByElement extends Element[ByElement]("by", parser = for {
  selector <- Attribute("selector").required
  directory <- Attribute("directory").optional
  list <- Attribute("list").optional
  stores <- StoreElement.allMustBe
} yield ByElement(
  selector,
  directory,
  list,
  stores
))
