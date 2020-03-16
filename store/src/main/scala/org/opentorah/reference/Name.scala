package org.opentorah.reference

import org.opentorah.xml.{Attribute, ContentType, Element, Parsable, Text, ToXml}
import scala.xml.Elem

final class Name private(
  entity: Entity,
  val id: Option[String],
  val name: String
) {
  def toXml: Elem = Name.parsable(entity).toXml(this)
}

object Name {

  def parsable(entity: Entity): Parsable[Name] with ToXml[Name] = new Element[Name](
    elementName = entity.nameElement,
    contentType = ContentType.Text,
    parser = for {
      id <- Attribute.id.optional
      name <- Text().required
    } yield new Name(entity, id, name)
  ) with ToXml[Name] {
    override def toXml(value: Name): Elem = {
      <name xml:id={value.id.orNull}>{value.name}</name>
        .copy(label = elementName)
    }
  }
}
