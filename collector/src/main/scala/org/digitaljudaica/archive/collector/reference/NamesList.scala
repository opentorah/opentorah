package org.digitaljudaica.archive.collector.reference

import org.digitaljudaica.reference.Entity
import org.digitaljudaica.xml.{ContentType, Parser, Xml}
import scala.xml.Elem

final class NamesList private(
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

object NamesList {

  final case class Descriptor(
    entity: Entity,
    id: String,
    role: Option[String],
    head: String
  ) {
    def fillOut(allNameds: Seq[Named]): NamesList = new NamesList(
      entity,
      id,
      role,
      head,
      nameds = allNameds.filter(named => named.entity == entity && named.role == role)
    )
  }

  val parser: Parser[Descriptor] = for {
    name <- Xml.name
    entity: Entity = Entity.forList(name).get
    id <- Xml.attribute.required.id
    role <- Xml.attribute.optional("role")
    head <- Xml.required("head", ContentType.Text, Xml.text.required)
  } yield Descriptor(
    entity,
    id,
    role,
    head
  )
}
