package org.digitaljudaica.archive.collector

import scala.xml.Elem

final case class Name(
  name: String,
  id: Option[String],
  entity: Entity
) {
  def toXml: Elem = {
    <name xml:id={id.orNull}>{name}</name>
      .copy(label = entity.nameElement)
  }
}
