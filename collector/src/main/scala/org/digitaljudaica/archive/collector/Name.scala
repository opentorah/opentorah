package org.digitaljudaica.archive.collector

import scala.xml.Elem
import Xml.Ops

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

object Name {
  def parseNames(entity: Entity, xml: Seq[Elem], errors: Errors): Seq[Name] = for (elem <- xml) yield {
    elem.check(entity.nameElement)

    Name(
      name = elem.text,
      id = elem.attributeOption("xml:id"),
      entity
    )
  }
}
