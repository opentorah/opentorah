package org.opentorah.collector

import org.opentorah.tei.{EntityType, Entity => TeiEntity}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, Parsable, Parser, Unparser, Xml}

final class Entity(
  override val name: String,
  val entityType: EntityType,
  val role: Option[String],
  val mainName: String
) extends Directory.Entry(name) with HtmlContent {
  def id: String = name

  override def viewer: Viewer = Viewer.Names
  override def htmlHeadTitle: Option[String] = Some(mainName)

  override def path(site: Site): Store.Path = Seq(site.entities, this)

  override def content(site: Site): Xml.Element = {
    val entity: TeiEntity = site.entities.getFile(this)

    val sources: Seq[Store] =
      for (reference <- site.getReferences.toId(id))
      yield site.resolve(reference.sourceUrl.get).get.last

    val fromEntities: Seq[Entity] = Collections.removeConsecutiveDuplicatesWith(
      for { source <- sources; if source.isInstanceOf[Entity] } yield source.asInstanceOf[Entity]
    )(_.id).sortBy(_.mainName)

    val fromDocuments: Map[Collection, Seq[Document.TextFacet]] =
      (for { source <- sources; if source.isInstanceOf[Document.TextFacet] } yield source.asInstanceOf[Document.TextFacet])
    .groupBy(_.collection)

    val byCollection: Seq[(Collection, Seq[Document.TextFacet])] =
      for {
        collection <- site.collections
        texts = fromDocuments.get(collection)
        if texts.isDefined
      } yield collection -> Collections.removeConsecutiveDuplicatesWith(texts.get)(_.document.name)
        .sortBy(_.document.name)

    val mentions: Xml.Element =
      <p class="mentions">
        {a(site)(text = "[...]")}
        {if (fromEntities.isEmpty) Seq.empty else
        <l>
          <em>{site.entityLists.selector.title.get}:</em>
          {Xml.multi(nodes = for (fromEntity <- fromEntities) yield fromEntity.a(site)(fromEntity.mainName))}
        </l>
        }
        {for ((collection, texts) <- byCollection) yield
        <l>{collection.pathHeaderHorizontal(site)}: {for (text <- texts) yield text.a(site)(text = text.document.baseName)}</l>}
      </p>

    TeiEntity.xmlElement(entity.copy(content = entity.content :+ mentions))
  }
}

// TODO use EntityRelated - but first generalize EntryMaker?
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
