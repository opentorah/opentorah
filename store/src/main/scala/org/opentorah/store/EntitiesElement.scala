package org.opentorah.store

import org.opentorah.entity.EntitiesList
import org.opentorah.xml.{Attribute, Element, ToXml}
import scala.xml.Elem

final case class EntitiesElement(
  selector: String,
  by: By.Element,
  lists: Seq[EntitiesList]
)

object EntitiesElement extends Element[EntitiesElement]("entities", parser = for {
  selector <- Attribute("selector").required
  by <- By.parsable.required
  lists <- EntitiesList.all
} yield EntitiesElement(
  selector,
  by,
  lists
)) with ToXml[EntitiesElement] {

  override def toXml(value: EntitiesElement): Elem =
    <entities selector={value.selector}>
      {By.parsable.toXml(value.by)}
      {EntitiesList.toXml(value.lists)}
    </entities>
}
