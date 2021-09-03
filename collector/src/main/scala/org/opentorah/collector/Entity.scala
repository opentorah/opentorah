package org.opentorah.collector

import org.opentorah.tei.{EntityRelated, EntityType, Entity => TeiEntity}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Caching, Directory}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, ContentType, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Entity(
  val entityType: EntityType,
  val role: Option[String],
  override val name: String,
  val mainName: String  // Note: can mostly be reconstructed from the name...
) extends Directory.Entry(name) with HtmlContent[Collector] {
  def id: String = name

  override def htmlHeadTitle: Option[String] = Some(mainName)

  def teiEntity(collector: Collector): Caching.Parser[TeiEntity] = collector.entities.getFile(this)

  override def content(collector: Collector): Caching.Parser[ScalaXml.Element] = for {
    entity <- teiEntity(collector)
    references <- collector.getReferences
    sources <- ZIO.foreach(
      references.filter(_.value.ref.contains(id))
    )(withSource => collector.resolveUrl(withSource.source).map(_.get.last))
  } yield {

    val fromEntities: Seq[Entity] = Collections.removeConsecutiveDuplicatesWith(
      sources.filter(_.isInstanceOf[Entity]).map(_.asInstanceOf[Entity])
    )(_.id).sortBy(_.mainName)

    val fromDocuments: Map[Collection, Seq[Document.TextFacet]] = sources
      .filter(_.isInstanceOf[Document.TextFacet])
      .map(_.asInstanceOf[Document.TextFacet])
      .groupBy(_.collection)

    val byCollection: Seq[(Collection, Seq[Document.TextFacet])] =
      for {
        collection <- collector.collections
        texts = fromDocuments.get(collection)
        if texts.isDefined
      } yield collection -> Collections.removeConsecutiveDuplicatesWith(texts.get)(_.document.name)
        .sortBy(_.document.name)

    val mentions: ScalaXml.Element =
      <p class="mentions">
        {a(collector)(text = "[...]")}
        {if (fromEntities.isEmpty) Seq.empty else
        <l>
          <em>{collector.entityLists.selector.title.get}:</em>
          {ScalaXml.multi(nodes = fromEntities.map(fromEntity => fromEntity.a(collector)(fromEntity.mainName)))}
        </l>
        }
        {for ((collection, texts) <- byCollection) yield
        <l>{collection.pathHeaderHorizontal(collector)}:
          {ScalaXml.multi(separator = " ", nodes = texts.map(text => text.a(collector)(text = text.document.baseName)))}</l>}
      </p>

    TeiEntity.xmlElement(entity.copy(content = entity.content :+ mentions))
  }
}

object Entity extends EntityRelated[Entity](
  elementName = _.element,
  entityType = _.entityType
) with Directory.EntryMaker[TeiEntity, Entity] {

  override def apply(name: String, entity: TeiEntity): Parser[Entity] = ZIO.succeed(new Entity(
    entity.entityType,
    entity.role,
    name,
    entity.names.head.name
  ))

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

  def line(entity: Entity, collector: Collector): ScalaXml.Element =
    <l>{entity.a(collector)(text = entity.mainName)}</l>

  def sort(entities: Seq[Entity]): Seq[Entity] = entities.sortBy(_.name)
}
