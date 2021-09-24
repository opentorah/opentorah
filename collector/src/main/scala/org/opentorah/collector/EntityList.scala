package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Caching, Store, Stores}
import org.opentorah.tei.{EntityRelated, EntityType, Title}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

// TODO derive it from By (with a transparent Selector)!
final class EntityList(
  override val fromUrl: Element.FromUrl,
  override val names: Names,
  val entityType: EntityType,
  val role: Option[String],
  val title: Title.Value,
) extends Store.NonTerminal, Stores.Pure, HtmlContent[Collector], Element.FromUrl.With:
  private var entities: Seq[Entity] = Seq.empty

  def setEntities(value: Seq[Entity]): Unit =
    entities = Entity.sort(value)

  def getEntities: Seq[Entity] = entities

  override def storesPure: Seq[Entity] = entities

  override def htmlHeadTitle: Option[String] = Some(title.content.toString)
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = Some(title.content.scalaXml)

  override def content(collector: Collector): Caching.Parser[ScalaXml.Element] = ZIO.succeed(
    <list>
      {getEntities.map(entity => Entity.line(entity, collector))}
    </list>
  )

object EntityList extends EntityRelated[EntityList](
  elementName = _.listElement,
  entityType = _.entityType
):
  override protected def contentType: Element.ContentType = Element.ContentType.Elements

  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional

  override protected def parsable(entityType: EntityType): Parsable[EntityList] = new Parsable[EntityList]:
    override def parser: Parser[EntityList] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      names: Names <- Names.withDefaultNameParsable()
      role: Option[String] <- roleAttribute()
      title: Title.Value <- Title.element.required()
    yield EntityList(
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
