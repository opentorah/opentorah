package org.opentorah.store

import org.opentorah.entity.EntitiesList
import org.opentorah.xml.{Attribute, Element, ToXml}
import scala.xml.Elem

// TODO merge into StoreElement - and generalize By creation into WithBy trait.
final case class EntitiesElement(
  selector: String,
  by: ByElement,
  lists: Seq[EntitiesList]
)

object EntitiesElement extends Element[EntitiesElement]("entities", parser = for {
  selector <- Attribute("selector").required
  by <- ByElement.required
  lists <- EntitiesList.all
} yield EntitiesElement(
  selector,
  by,
  lists
)) with ToXml[EntitiesElement] {

  override def toXml(value: EntitiesElement): Elem =
    <entities selector={value.selector}>
      {ByElement.toXml(value.by)}
      {EntitiesList.toXml(value.lists)}
    </entities>
}
