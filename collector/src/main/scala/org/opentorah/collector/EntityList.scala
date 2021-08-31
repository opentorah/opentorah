package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Caching, Store, Stores}
import org.opentorah.tei.{EntityRelated, EntityType, Title}
import org.opentorah.xml.{Attribute, ContentType, Element, FromUrl, Parsable, Parser, ScalaXml, Unparser}

// TODO derive it from By (with a transparent Selector)!
final class EntityList(
  override val fromUrl: FromUrl,
  override val names: Names,
  val entityType: EntityType,
  val role: Option[String],
  val title: Title.Value,
) extends Store.NonTerminal with Stores.Terminal with HtmlContent[Collector] with FromUrl.With {
  private var entities: Seq[Entity] = Seq.empty

  def setEntities(value: Seq[Entity]): Unit = {
    entities = Entity.sort(value)
  }

  def getEntities: Seq[Entity] = entities

  override protected def terminalStores: Seq[Store.Terminal] = entities

  override def htmlHeadTitle: Option[String] = Some(ScalaXml.toString(title.content))
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = Some(title.content)

  override def content(collector: Collector): Caching.Parser[ScalaXml.Element] =
    for {_ <- collector.entityLists.setUp(collector)} yield
      <list>
        {getEntities.map(entity => Entity.line(entity, collector))}
      </list>
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
