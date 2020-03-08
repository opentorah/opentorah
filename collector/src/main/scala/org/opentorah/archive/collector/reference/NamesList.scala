package org.opentorah.archive.collector.reference

import org.opentorah.reference.Entity
import scala.xml.Elem

final class NamesList(
  storeNamesList: org.opentorah.reference.NamesList,
  val nameds: Seq[Named]
) {
  def entity: Entity = storeNamesList.entity
  def id: String = storeNamesList.id
  def role: Option[String] = storeNamesList.role
  def head: String = storeNamesList.head

  def isEmpty: Boolean = nameds.isEmpty

  def references: Seq[Reference] = nameds.flatMap(_.references)

  def toXml: Elem =
    <list xml:id={id} role={role.orNull}>
      <head>{head}</head>
      {for (named <- nameds) yield named.toListXml}
    </list>
      .copy(label = entity.listElement)
}
