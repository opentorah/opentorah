package org.opentorah.store

import org.opentorah.xml.{Attribute, Element, ToXml}
import scala.xml.Elem

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
)) with ToXml[ByElement] {

  override def toXml(value: ByElement): Elem =
    <by
      selector={value.selector}
      directory={value.directory.orNull}
      list={value.list.orNull}
    >
      {StoreElement.toXml(value.stores)}
    </by>
}
