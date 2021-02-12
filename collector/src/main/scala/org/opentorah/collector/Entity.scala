package org.opentorah.collector

import org.opentorah.tei.{EntityRelated, EntityType, Entity => TeiEntity}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, ContentType, Parsable, Parser, Unparser, Xml}

final class Entity(
  val entityType: EntityType,
  val role: Option[String],
  override val name: String,
  val mainName: String  // Note: can mostly be reconstructed from the name...
) extends Directory.Entry(name) with HtmlContent {
  def id: String = name

  override def viewer: Viewer = Viewer.Names
  override def htmlHeadTitle: Option[String] = Some(mainName)

  def teiEntity(site: Site): TeiEntity = site.entities.getFile(this)

  override def path(site: Site): Store.Path = Seq(site.entities, this)

  override def content(site: Site): Xml.Element = {
    val sources: Seq[Store] = WithSource.resolve(site, site.getReferences.filter(_.value.ref.contains(id)))

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

    val entity: TeiEntity = teiEntity(site)
    TeiEntity.xmlElement(entity.copy(content = entity.content :+ mentions))
  }
}

object Entity extends EntityRelated[Entity](
  elementName = _.element,
  entityType = _.entityType
) with Directory.EntryMaker[TeiEntity, Entity] {

  override def apply(name: String, entity: TeiEntity): Entity = new Entity(
    entity.entityType,
    entity.role,
    name,
    entity.names.head.name
  )

  override protected def contentType: ContentType = ContentType.Elements

  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional
  private val mainNameAttribute: Attribute.Required[String] = Attribute("name").required

  override protected def parsable(entityType: EntityType): Parsable[Entity] = new Parsable[Entity] {
    override def parser: Parser[Entity] = for {
      role <- roleAttribute()
      name <- Directory.fileNameAttribute()
      mainName <- mainNameAttribute()
    } yield new Entity(
      entityType,
      role,
      name,
      mainName
    )

    override def unparser: Unparser[Entity] = Unparser.concat(
      roleAttribute(_.role),
      Directory.fileNameAttribute(_.name),
      mainNameAttribute(_.mainName)
    )
  }
}
