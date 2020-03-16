package org.opentorah.archive.collector.reference

import org.opentorah.reference.Entity
import scala.xml.Node

final case class Reference(
  source: ReferenceSource,
  reference: org.opentorah.reference.Reference
) {
  override def toString: String = source.toString

  def entity: Entity = reference.entity
  def name: Seq[Node] = reference.name
  def id: Option[String] = reference.id
  def role: Option[String] = reference.role
  def ref: Option[String] = reference.ref
}
