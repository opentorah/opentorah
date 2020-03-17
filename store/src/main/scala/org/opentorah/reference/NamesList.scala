package org.opentorah.reference

import org.opentorah.xml.{Attribute, ContentType, Parsable, Parser, Text, ToXml}
import scala.xml.Elem

final case class NamesList(
  entity: Entity,
  id: String,
  role: Option[String],
  head: String,
  nameds: Seq[Named]
) {
  def take(nameds: Seq[Named]): NamesList = copy(nameds = nameds.filter(includes))

  def includes(named: Named): Boolean = (named.entity == entity) && (named.role == role)

  def isEmpty: Boolean = nameds.isEmpty

  def references: Seq[Reference] = nameds.flatMap(_.references)
}

object NamesList extends Parsable[NamesList] with ToXml[NamesList] {

  override def toString: String = "NamesList"

  override val name2parser: Map[String, Parsable.ContentTypeAndParser[NamesList]] = Entity.values.map { entity =>
    entity.listElement -> new Parsable.ContentTypeAndParser[NamesList](ContentType.Elements, parser(entity))
  }.toMap

  private def parser(entity: Entity): Parser[NamesList] = for {
    id <- Attribute.id.required
    role <- Attribute("role").optional
    head <- Text("head").required
  } yield NamesList(
    entity,
    id,
    role,
    head,
    Seq.empty
  )

  override def toXml(value: NamesList): Elem =
    <elem id={value.id} role={value.role.orNull}>
      <head>{value.head}</head>
    </elem>
    .copy(label = value.entity.listElement)
}
