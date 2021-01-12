package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.{EntityRelated, EntityType, Title}
import org.opentorah.xml.{Unparser, Attribute, ContentType, Element, FromUrl, Parsable, Parser}

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

  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional

  override protected def parsable(entityType: EntityType): Parsable[EntityList] = new Parsable[EntityList] {
    override def parser: Parser[EntityList] = for {
      fromUrl <- Element.currentFromUrl
      names <- Names.withDefaultNameParsable()
      role <- roleAttribute()
      title <- Title.element.required()
    } yield new EntityList(
      fromUrl,
      names,
      entityType,
      role,
      title
    )

    override def unparser: Unparser[EntityList] = Unparser.concat(
      Names.withDefaultNameParsable(_.names),
      roleAttribute(_.role),
      Title.element.required(_.title),
    )
  }
}
