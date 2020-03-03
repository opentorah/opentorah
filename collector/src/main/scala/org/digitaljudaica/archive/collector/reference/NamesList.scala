package org.digitaljudaica.archive.collector.reference

import org.digitaljudaica.reference.Entity
import scala.xml.Elem

final class NamesList(
  val entity: Entity,
  val id: String,
  val role: Option[String],
  val head: String,
  val nameds: Seq[Named]
) {

  def isEmpty: Boolean = nameds.isEmpty

  def references: Seq[Reference] = nameds.flatMap(_.references)

  def toXml: Elem =
    <list xml:id={id} role={role.orNull}>
      <head>{head}</head>
      {for (named <- nameds) yield named.toListXml}
    </list>
      .copy(label = entity.listElement)
}
