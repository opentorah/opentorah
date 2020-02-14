package org.digitaljudaica.archive.collector.reference

import org.digitaljudaica.xml.Ops._
import scala.xml.Elem

final class NamesList(
  xml: Elem,
  allNameds: Seq[Named]
) {

  val entity: Entity = Entity.forList(xml.label).get
  val role: Option[String] = xml.attributeOption("role")
  val id: String = xml.id
  val head: String = xml.oneChild("head").text
  val nameds: Seq[Named] = allNameds.filter(named => named.entity == entity && named.role == role)

  def isEmpty: Boolean = nameds.isEmpty

  def references: Seq[Reference] = nameds.flatMap(_.references)

  def toXml: Elem =
    <list xml:id={id} role={role.orNull}>
      <head>{head}</head>
      {for (named <- nameds) yield named.toListXml}
    </list>
      .copy(label = entity.listElement)
}
