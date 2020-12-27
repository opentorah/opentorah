package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.{EntityRelated, EntityType, Title}
import org.opentorah.xml.{Antiparser, Attribute, ContentType, FromUrl, Parser}

final class EntityList(
  override val fromUrl: FromUrl,
  override val names: Names,
  val entityType: EntityType,
  val role: Option[String],
  val title: Title.Value,
) extends Store with FromUrl.With {
  override def findByName(name: String): Option[Store] = None
}

object EntityList extends EntityRelated[EntityList](
  elementName = _.listElement,
  entityType = _.entityType
) {
  override protected def contentType: ContentType = ContentType.Elements

  private val roleAttribute: Attribute[String] = Attribute("role")

  override def parser(entityType: EntityType): Parser[EntityList] = for {
    fromUrl <- currentFromUrl
    names <- Names.withDefaultNameParser
    role <- roleAttribute.optional
    title <- Title.parsable.required
  } yield new EntityList(
    fromUrl,
    names,
    entityType,
    role,
    title
  )

  override def antiparser(entityType: EntityType): Antiparser[EntityList] = Antiparser.concat(
    Names.antiparser(_.names),
    roleAttribute.toXmlOption(_.role),
    Title.parsable.toXml(_.title),
  )
}
