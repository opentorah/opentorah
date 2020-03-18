package org.opentorah.store

import org.opentorah.xml.{Attribute, Element}

final case class ByElement(
  selector: String,
  files: Seq[String]
)

object ByElement extends Element[ByElement]("by", parser = for {
  selector <- Attribute("selector").required
  files <- ByElement.storeParsable.allMustBe
} yield ByElement(
  selector,
  files
)) {
  object storeParsable extends Element[String]("store", parser = Attribute("file").required)
}
