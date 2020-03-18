package org.opentorah.reference

import org.opentorah.xml.{Attribute, ContentType, Parsable, Parser, Text, ToXml}
import scala.xml.Elem

final case class NamedsList(
  entity: Entity,
  id: String,
  role: Option[String],
  head: String,
  nameds: Seq[Named]
) {
  def take(nameds: Seq[Named]): NamedsList = copy(nameds = nameds.filter(includes))

  def includes(named: Named): Boolean = (named.entity == entity) && (named.role == role)

  def isEmpty: Boolean = nameds.isEmpty
}

object NamedsList extends Parsable[NamedsList] with ToXml[NamedsList] {

  override def toString: String = "NamedsList"

  override val name2parser: Map[String, Parsable.ContentTypeAndParser[NamedsList]] = Entity.values.map { entity =>
    entity.listElement -> new Parsable.ContentTypeAndParser[NamedsList](ContentType.Elements, parser(entity))
  }.toMap

  private def parser(entity: Entity): Parser[NamedsList] = for {
    id <- Attribute.id.required
    role <- Attribute("role").optional
    head <- Text("head").required
  } yield NamedsList(
    entity,
    id,
    role,
    head,
    Seq.empty
  )

  override def toXml(value: NamedsList): Elem =
    <elem id={value.id} role={value.role.orNull}>
      <head>{value.head}</head>
    </elem>
    .copy(label = value.entity.listElement)
}
