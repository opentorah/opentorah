package org.opentorah.collectorng

import org.opentorah.tei.{Entity => TeiEntity, EntityType}
import org.opentorah.xml.{Antiparser, Attribute, Parser}

final class Entity(
  override val name: String,
  val entityType: EntityType,
  val role: Option[String],
  val mainName: String
) extends Directory.Entry(name)

object Entity extends Directory.EntryMaker[TeiEntity, Entity]("entity") {

  override def apply(name: String, entity: TeiEntity): Entity = new Entity(
    name,
    entity.entityType,
    entity.role,
    entity.names.head.name
  )

  private val entityTypeAttribute: Attribute[String] = Attribute("type")
  private val roleAttribute: Attribute[String] = Attribute("role")
  private val mainNameAttribute: Attribute[String] = Attribute("name")

  override def parser: Parser[Entity] = for {
    name <- Directory.fileName
    entityType <- entityTypeAttribute.required.map(value => EntityType.values.find(_.element == value).get)
    role <- roleAttribute.optional
    mainName <- mainNameAttribute.required
  } yield new Entity(
    name,
    entityType,
    role,
    mainName
  )

  override def antiparser: Antiparser[Entity] = Antiparser.concat(
    Directory.fileNameToXml,
    entityTypeAttribute.toXml(_.entityType.element),
    roleAttribute.toXmlOption(_.role),
    mainNameAttribute.toXml(_.mainName)
  )
}
