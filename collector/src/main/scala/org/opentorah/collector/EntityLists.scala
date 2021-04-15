package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{EntityRelated, EntityType, Tei, Title}
import org.opentorah.site.{By, Caching, HtmlContent, Selector, Store}
import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, ContentType, Element, FromUrl, Parsable, Parser, Unparser, Xml}

final class EntityLists(
  override val selector: Selector,
  val lists: Seq[EntityLists.EntityList]
) extends By with HtmlContent[Site] {
  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[Xml.Nodes] = htmlHeadTitle.map(Xml.mkText)

  private var list2entities: Option[Map[EntityLists.EntityList, Seq[Entity]]] = None
  private var nonEmptyLists: Option[Seq[EntityLists.EntityList]] = None

  private def setUp(site: Site): Caching.Parser[Unit] = if (list2entities.nonEmpty) Effects.ok else
    site.entities.directoryEntries.map { allEntities =>
      val result: Map[EntityLists.EntityList, Seq[Entity]] = (for (list <- lists) yield {
        val entities = allEntities.filter(entity =>
          (entity.entityType == list.entityType) &&
          (entity.role       == list.role      )
        ).sortBy(_.name)
        list -> entities
      }).toMap

      list2entities = Some(result)

      nonEmptyLists = Some(lists.filterNot(list => result(list).isEmpty))
    }

  override def content(site: Site): Caching.Parser[Xml.Element] = for { _ <- setUp(site) } yield {
    <div>
      <p>{nonEmptyLists.get.map(list => <l>{a(site).setFragment(list.names.name)(xml = list.title)}</l>)}</p>
      {nonEmptyLists.get.map(list =>
      <list id={list.names.name}>
        <head xmlns={Tei.namespace.uri}>{list.title.xml}</head>
        {list2entities.get(list).map(entity => <l>{entity.a(site)(text = entity.mainName)}</l>)}
      </list>
    )}</div>
  }
}

object EntityLists extends Element[EntityLists]("entityLists") {

  final class EntityList(
    override val fromUrl: FromUrl,
    override val names: Names,
    val entityType: EntityType,
    val role: Option[String],
    val title: Title.Value,
  ) extends Store with FromUrl.With

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

  override def contentParsable: Parsable[EntityLists] = new Parsable[EntityLists] {
    override def parser: Parser[EntityLists] = for {
      selector <- By.selectorParser
      lists <- EntityList.seq()
    } yield new EntityLists(
      selector,
      lists
    )

    override def unparser: Unparser[EntityLists] = Unparser.concat(
      By.selectorUnparser,
      EntityList.seq(_.lists)
    )
  }
}
