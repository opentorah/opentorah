package org.opentorah.collector

import org.opentorah.tei.{EntityReference, EntityRelated, EntityType, Entity as TeiEntity}
import org.opentorah.store.{Context, Directory, ListFile, Path, Store, Viewer, WithSource}
import org.opentorah.util.{Collections, Files}
import org.opentorah.xml.{Attribute, Caching, Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO
import java.net.URL

final class Entity(
  val entityType: EntityType,
  val role: Option[String],
  override val name: String,
  val mainName: String  // Note: can mostly be reconstructed from the name...
) extends
  Directory.Entry(name),
  Viewer.Apparatus derives CanEqual:

  override def equals(other: Any): Boolean =
    val that: Entity = other.asInstanceOf[Entity]
    this.name == that.name

  def id: String = name

  private def references(referencesRootUrl: URL): ListFile[WithSource[EntityReference], Seq[WithSource[EntityReference]]] = WithSource(
    url = Files.fileInDirectory(referencesRootUrl, s"$id.xml"),
    name = "references",
    value = EntityReference
  )

  def writeReferences(allReferences: Seq[WithSource[EntityReference]], collector: Collector): Unit =
    references(collector.enityReferencesUrl).write(
      allReferences.filter(_.value.ref.contains(id)).sortBy(_.source)
    )

  override def htmlHeadTitle: Option[String] = Some(mainName)
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = None

  def getTei(collector: Collector): Caching.Parser[TeiEntity] = collector.entities.getFile(this)

  // TODO dissolve?
  def line(context: Context, pathShortener: Path.Shortener): ScalaXml.Element =
    <l>{reference(context, pathShortener)}</l>

  private def reference(context: Context, pathShortener: Path.Shortener) =
    a(context.path(this), pathShortener)(mainName)

  override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] =
    val collector: Collector = Collector.get(context)
    for
      entity: TeiEntity <- getTei(collector)

      references: Seq[WithSource[EntityReference]] <- references(collector.enityReferencesUrl).get
      sources: Seq[Store] <- ZIO.foreach(references)((withSource: WithSource[EntityReference]) =>
        collector.resolveUrl(withSource.source).map(_.get.last))
      collectionPaths: Seq[Path] <- collector.collectionPaths
      pathShortener: Path.Shortener <- context.pathShortener
    yield

      val fromEntities: Seq[Entity] = Collections.removeConsecutiveDuplicatesWith(
        sources.filter(_.isInstanceOf[Entity]).map(_.asInstanceOf[Entity])
      )(_.id).sortBy(_.mainName)

      val fromDocuments: Map[Collection, Seq[TextFacet]] = sources
        .filter(_.isInstanceOf[TextFacet]).map(_.asInstanceOf[TextFacet])
        .groupBy(_.collection)

      val byCollection: Seq[(Collection, (Path, Seq[TextFacet]))] = for
        path: Path <- collectionPaths
        collection: Collection = Path.last[Collection](path)
        textsOpt: Option[Seq[TextFacet]] = fromDocuments.get(collection)
        if textsOpt.isDefined
        texts: Seq[TextFacet] = Collections.removeConsecutiveDuplicatesWith(textsOpt.get)(_.document.name).sortBy(_.document.name)
      yield collection -> (path, texts)

      val mentions: ScalaXml.Element =
        <p class="mentions">
          {if fromEntities.isEmpty then Seq.empty else
          <l>
            <em>{collector.entityLists.selector.title.get}:</em>
            {ScalaXml.multi(nodes = for fromEntity <- fromEntities yield fromEntity.reference(context, pathShortener))}
          </l>
          }
          {for (collection: Collection, (collectionPath: Path, texts: Seq[TextFacet])) <- byCollection yield
            <l>{
              collection.pathHeaderHorizontal(collectionPath)
            }: {ScalaXml.multi(separator = " ", nodes =
              for text: TextFacet <- texts yield
                text.document.textFacetLink(collectionPath, pathShortener)(text = text.document.baseName))
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

  def sort(entities: Seq[Entity]): Seq[Entity] = entities.sortBy(_.name)
