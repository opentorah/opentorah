package org.opentorah.collectorng

import org.opentorah.tei.{EntityType, Entity => TeiEntity}
import org.opentorah.xml.{Unparser, Attribute, Parsable, Parser}

// TODO use Union to record org/place/person...
final class Entity(
  override val name: String,
  val entityType: EntityType,
  val role: Option[String],
  val mainName: String
) extends Directory.Entry(name) {
  def id: String = name
}

object Entity extends Directory.EntryMaker[TeiEntity, Entity]("entity") {

  override def apply(name: String, entity: TeiEntity): Entity = new Entity(
    name,
    entity.entityType,
    entity.role,
    entity.names.head.name
  )

  private val entityTypeAttribute: Attribute.Required[String] = Attribute("type").required
  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional
  private val mainNameAttribute: Attribute.Required[String] = Attribute("name").required

  override def contentParsable: Parsable[Entity] = new Parsable[Entity] {
    override def parser: Parser[Entity] = for {
      name <- Directory.fileNameAttribute()
      entityType <- entityTypeAttribute().map(value => EntityType.values.find(_.element == value).get)
      role <- roleAttribute()
      mainName <- mainNameAttribute()
    } yield new Entity(
      name,
      entityType,
      role,
      mainName
    )

    override def unparser: Unparser[Entity] = Unparser.concat(
      Directory.fileNameAttribute(_.name),
      entityTypeAttribute(_.entityType.element),
      roleAttribute(_.role),
      mainNameAttribute(_.mainName)
    )
  }
}
