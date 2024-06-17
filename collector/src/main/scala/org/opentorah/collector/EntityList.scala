package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.store.{Context, Path, Pure}
import org.opentorah.tei.{EntityRelated, EntityType, Title}
import org.opentorah.xml.{Attribute, ContentType, Element, Elements, FromUrl, Nodes, Parsable, Parser, Unparser}
import zio.ZIO

// TODO derive it from By (with a transparent Selector)!
final class EntityList(
  override val fromUrl: FromUrl,
  override val names: Names,
  val entityType: EntityType,
  val role: Option[String],
  val title: Title.Value,
) extends
  Pure[Entity],
  FromUrl.With:
  private var entities: Seq[Entity] = Seq.empty

  def setEntities(value: Seq[Entity]): Unit =
    entities = Entity.sort(value)

  def getEntities: Seq[Entity] = entities

  override def storesPure: Seq[Entity] = entities

  override def htmlHeadTitle: Option[String] = Some(title.content.toString)
  override def htmlBodyTitle: Option[Nodes] = Some(title.content)

  override def content(path: Path, context: Context): Parser[Element] =
    for lines: Elements <- ZIO.foreach(getEntities)((entity: Entity) =>
      entity.line(context)
    )
    yield <list>{lines}</list>

object EntityList extends EntityRelated[EntityList](
  elementName = _.listElement,
  entityType = _.entityType
):
  override protected def contentType: ContentType = ContentType.Elements

  override protected def parsable(entityType: EntityType): Parsable[EntityList] = new Parsable[EntityList]:
    private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional

    override def parser: Parser[EntityList] = for
      fromUrl: FromUrl <- FromUrl.get
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
