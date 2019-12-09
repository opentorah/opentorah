package org.digitaljudaica.archive.collector

import scala.xml.Elem
import Xml.Ops

final case class NamesList(
  id: String,
  head: String,
  entity: Entity,
  role: Option[String],
  nameds: Seq[Named]
) {
  def isEmpty: Boolean = nameds.isEmpty

  def references: Seq[Reference] = nameds.flatMap(_.references)

  def addMentions(references: Seq[Reference]): NamesList =
    copy(nameds = nameds.map(_.addMentions(references)))

  def toXml: Elem =
    <list xml:id={id}>
      <head>{head}</head>
      {for (named <- nameds) yield named.toXml}
    </list>
    .copy(label = entity.listElement)
}

object NamesList {

  def apply(xml: Elem, nameds: Seq[Named]): NamesList = {
    val entity: Entity = Entity.forList(xml.label).get
    val role: Option[String] = xml.attributeOption("role")

    NamesList(
      id = xml.id,
      head = xml.oneChild("head").text,
      entity,
      role,
      nameds = nameds.filter(named => named.entity == entity && named.role == role)
    )
  }
}
