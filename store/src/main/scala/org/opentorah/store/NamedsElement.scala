package org.opentorah.store

import org.opentorah.reference.NamedsList
import org.opentorah.xml.{Attribute, Element, Text}

final case class NamedsElement(
  selector: String,
  by: String,
  directory: String,
  list: String,
  lists: Seq[NamedsList]
)

object NamedsElement extends Element[NamedsElement]("nameds", parser = for {
  selector <- Attribute("selector").required
  by <- Attribute("by").required
  directory <- Attribute("directory").required
  list <- Attribute("list").required
  lists <- NamedsList.all
} yield NamedsElement(
  selector,
  by,
  directory,
  list,
  lists
))
