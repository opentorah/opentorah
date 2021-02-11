package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{EntityRelated, EntityType, Tei, Title}
import org.opentorah.xml.{Attribute, ContentType, Element, FromUrl, Parsable, Parser, Unparser, Xml}

final class EntityLists(
  override val selector: Selector,
  val lists: Seq[EntityLists.EntityList]
) extends By with HtmlContent {
  override def viewer: Viewer = Viewer.Names
  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[Xml.Nodes] = htmlHeadTitle.map(Xml.mkText)

  private var list2entities: Option[Map[EntityLists.EntityList, Seq[Entity]]] = None

  private def setUp(site: Site): Unit = if (list2entities.isEmpty) list2entities = Some((for (list <- lists) yield {
    val entities = site.entities.directoryEntries.filter(entity =>
      (entity.entityType == list.entityType) &&
      (entity.role       == list.role      )
    ).sortBy(_.name)
    list -> entities
  }).toMap)

  private def getEntities(list: EntityLists.EntityList): Seq[Entity] = list2entities.get(list)

  private lazy val nonEmptyLists: Seq[EntityLists.EntityList] = lists.filterNot(list => getEntities(list).isEmpty)

  override def path(site: Site): Store.Path = Seq(site.entityLists)

  override def content(site: Site): Xml.Element = {
    setUp(site)

    <div>
      <p>
        {for (list <- nonEmptyLists) yield <l>{a(site, part = Some(list.names.name))(xml = list.title)}</l>}
      </p>
      {for (list <- nonEmptyLists) yield
      <list id={list.names.name}>
        <head xmlns={Tei.namespace.uri}>{list.title.xml}</head>
        {for (entity <- getEntities(list)) yield <l>{entity.a(site)(text = entity.mainName)}</l>}
      </list>
      }
    </div>
  }
}

object EntityLists extends Element[EntityLists]("entityLists") {

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
