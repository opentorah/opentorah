package org.opentorah.collector

import org.opentorah.html.A
import org.opentorah.tei.{EntityReference, EntityRelated, EntityType, Entity as TeiEntity}
import org.opentorah.store.{Context, Directory, ListFile, Path, Store, WithSource}
import org.opentorah.util.{Collections, Files}
import org.opentorah.xml.{Attribute, ContentType, Element, Elements, Nodes, Parsable, Parser, Unparser}
import zio.ZIO
import java.net.URL

final class Entity(
  val entityType: EntityType,
  val role: Option[String],
  override val name: String,
  val mainName: String  // Note: can mostly be reconstructed from the name...
) extends Directory.Entry(name) derives CanEqual:

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
  override def htmlBodyTitle: Option[Nodes] = None

  def getTei(collector: Collector): Parser[TeiEntity] = collector.entities.getFile(this)

  // TODO dissolve?
  def line(context: Context): Parser[Element] =
    for result <- reference(context) yield <l>{result}</l>

  // TODO eliminate
  private def reference(context: Context): Parser[Element] =
    for a: A <- context.a(this) yield a(mainName)

  override def content(path: Path, context: Context): Parser[Element] =
    val collector: Collector = Collector.get(context)
    for
      entity: TeiEntity <- getTei(collector)

      references: Seq[WithSource[EntityReference]] <- references(collector.enityReferencesUrl).get
      sources: Seq[Store] <- ZIO.foreach(references)((withSource: WithSource[EntityReference]) =>
        collector.resolveUrl(withSource.source).map(_.get.last))
      collectionPaths: Seq[Path] <- collector.collectionPaths

      fromEntities: Seq[Entity] = Collections.removeConsecutiveDuplicatesWith(
        sources.filter(_.isInstanceOf[Entity]).map(_.asInstanceOf[Entity])
      )(_.id).sortBy(_.mainName)

      fromEntitiesResult: Elements <- ZIO.foreach(fromEntities)((fromEntity: Entity) =>
        fromEntity.reference(context)
      )

      fromDocuments: Map[Collection, Seq[TextFacet]] = sources
        .filter(_.isInstanceOf[TextFacet]).map(_.asInstanceOf[TextFacet])
        .groupBy(_.collection)

      byCollection: Seq[(Collection, (Path, Seq[TextFacet]))] = for
        path: Path <- collectionPaths
        collection: Collection = Path.last[Collection](path)
        textsOpt: Option[Seq[TextFacet]] = fromDocuments.get(collection)
        if textsOpt.isDefined
        texts: Seq[TextFacet] = Collections.removeConsecutiveDuplicatesWith(textsOpt.get)(_.document.name).sortBy(_.document.name)
      yield collection -> (path, texts)

      fromCollectionsResult: Elements <- ZIO.foreach(byCollection){
        case (collection: Collection, (collectionPath: Path, texts: Seq[TextFacet])) =>
          for pageLinks: Nodes <- ZIO.foreach(texts)(
            (text: TextFacet) =>
              for textFacetA: A <- text.document.textFacetLink(context, collectionPath)
              yield textFacetA(text = text.document.baseName)
            )
          yield
            <l>
              {collection.pathHeaderHorizontal(collectionPath)}: {Nodes.multi(separator = " ", nodes = pageLinks)}
            </l>
      }

    yield

      val mentions: Element =
        <p class="mentions">
          {if fromEntities.isEmpty then Seq.empty else
          <l>
            <em>{collector.entityLists.selector.title.get}:</em>
            {Nodes.multi(nodes = fromEntitiesResult)}
          </l>
          }
          {fromCollectionsResult}
        </p>

      TeiEntity.xmlElement(entity.copy(content = entity.content :+ mentions))

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

  override protected def contentType: ContentType = ContentType.Elements

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
