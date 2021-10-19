package org.opentorah.collector

import org.opentorah.tei.{EntityReference, EntityRelated, EntityType, Entity as TeiEntity}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Directory, Path, Store, WithSource}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, Caching, Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Entity(
  val entityType: EntityType,
  val role: Option[String],
  override val name: String,
  val mainName: String  // Note: can mostly be reconstructed from the name...
) extends 
  Directory.Entry(name),
  HtmlContent.ApparatusViewer[Collector] derives CanEqual:
  
  override def equals(other: Any): Boolean =
    val that: Entity = other.asInstanceOf[Entity]
    this.name == that.name

  def id: String = name

  override def htmlHeadTitle: Option[String] = Some(mainName)

  def getTei(collector: Collector): Caching.Parser[TeiEntity] = collector.entities.getFile(this)

  override def content(path: Path, collector: Collector): Caching.Parser[ScalaXml.Element] = for
    entity: TeiEntity <- getTei(collector)
    references: Seq[WithSource[EntityReference]] <- collector.getReferences
    sources: Seq[Store] <- ZIO.foreach(
      references.filter(_.value.ref.contains(id))
    )((withSource: WithSource[EntityReference]) => collector.resolveUrl(withSource.source).map(_.get.last))
    collectionPaths: Seq[Path] <- collector.collectionPaths
  yield

    val fromEntities: Seq[Entity] = Collections.removeConsecutiveDuplicatesWith(
      sources.filter(_.isInstanceOf[Entity]).map(_.asInstanceOf[Entity])
    )(_.id).sortBy(_.mainName)

    val fromDocuments: Map[Collection, Seq[TextFacet]] = sources
      .filter(_.isInstanceOf[TextFacet]).map(_.asInstanceOf[TextFacet])
      .groupBy(_.collection)

    val byCollection: Seq[(Collection, (Path, Seq[TextFacet]))] =
      for
        path: Path <- collectionPaths
        collection: Collection = path.last.asInstanceOf[Collection]
        textsOpt: Option[Seq[TextFacet]] = fromDocuments.get(collection)
        if textsOpt.isDefined
        texts: Seq[TextFacet] = Collections.removeConsecutiveDuplicatesWith(textsOpt.get)(_.document.name).sortBy(_.document.name)
      yield collection -> (path, texts)

    // TODO submerge Document contribution in Document...
    val mentions: ScalaXml.Element =
      <p class="mentions">
        {if fromEntities.isEmpty then Seq.empty else
        <l>
          <em>{collector.entityLists.selector.title.get}:</em>
          {ScalaXml.multi(nodes = for fromEntity <- fromEntities yield Entity.reference(fromEntity, collector))}
        </l>
        }
        {for (collection: Collection, (collectionPath: Path, texts: Seq[TextFacet])) <- byCollection yield
          <l>{
            collection.pathHeaderHorizontal(collectionPath)
          }: {ScalaXml.multi(separator = " ", nodes =
            for text: TextFacet <- texts yield
              text.document.textFacetLink(collectionPath, collector)(text = text.document.baseName))
          }</l>
        }</p>

    TeiEntity.xmlElement(entity.copy(content = ScalaXml.toNodes(entity.content.scalaXml :+ mentions)))

object Entity extends EntityRelated[Entity](
  elementName = _.element,
  entityType = _.entityType
), Directory.EntryMaker[TeiEntity, Entity]:

  override def apply(name: String, entity: TeiEntity): Parser[Entity] = ZIO.succeed(new Entity(
    entity.entityType,
    entity.role,
    name,
    entity.names.head.name
  ))

  override protected def contentType: Element.ContentType = Element.ContentType.Elements

  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional
  private val mainNameAttribute: Attribute.Required[String] = Attribute("name").required

  override protected def parsable(entityType: EntityType): Parsable[Entity] = new Parsable[Entity]:
    override def parser: Parser[Entity] = for
      role: Option[String] <- roleAttribute()
      name: String <- Directory.fileNameAttribute()
      mainName: String <- mainNameAttribute()
    yield new Entity(
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

  // TODO move into the Entity class?
  def line(entity: Entity, collector: Collector): ScalaXml.Element =
    <l>{reference(entity, collector)}</l>

  private def reference(entity: Entity, collector: Collector) =
    collector.a(collector.entityPath(entity))(entity.mainName)

  def sort(entities: Seq[Entity]): Seq[Entity] = entities.sortBy(_.name)
