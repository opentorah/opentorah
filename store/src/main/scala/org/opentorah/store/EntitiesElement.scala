package org.opentorah.store

import org.opentorah.entity.EntitiesList
import org.opentorah.xml.{Attribute, Element}

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
))
