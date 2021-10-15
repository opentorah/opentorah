package org.opentorah.collector

import org.opentorah.html
import org.opentorah.html.Html
import org.opentorah.site.{HtmlContent, Site, SiteCommon}
import org.opentorah.store.{Caching, Directory, ListFile, Store, WithSource}
import org.opentorah.tei.{EntityReference, EntityType, LinksResolver, Tei, Unclear}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Element, Parser, ScalaXml}
import zio.{UIO, ZIO}
import java.net.URL

// TODO retrieve TEI(?) references from notes.
final class Collector(
  fromUrl: Element.FromUrl,
  common: SiteCommon,
  val entities: Entities,
  val entityLists: EntityLists,
  val notes: Notes,
  val by: ByHierarchy
) extends Site[Collector](
  fromUrl,
  common
):
  private val paths: Seq[Store.Path] = getPaths(Seq.empty, by)

  private def getPaths(path: Store.Path, by: ByHierarchy): Seq[Store.Path] =
    Effects.unsafeRun(toTask(by.stores)).flatMap { store =>
    val storePath: Store.Path = path ++ Seq(by, store)
    Seq(storePath) ++ (store match
      case hierarchy : Hierarchy  => getPaths(storePath, hierarchy.by)
      case _ => Seq.empty
    )
  }

  // TODO eliminate
  val store2path: Map[Store, Store.Path] = paths.map(path => path.last -> path).toMap

  val collections: Seq[Collection] = for
    path: Store.Path <- paths
    last: Store = path.last
    if last.isInstanceOf[Collection]
  yield last.asInstanceOf[Collection]

  private val hierarchies: Seq[Hierarchy] = for
    path: Store.Path <- paths
    last: Store = path.last
    if last.isInstanceOf[Hierarchy]
  yield last.asInstanceOf[Hierarchy]

  private val alias2collectionAlias: Map[String, Collection.Alias] = collections
    .filter(_.alias.isDefined)
    .map(collection => collection.alias.get -> Collection.Alias(collection))
    .toMap

  private val references: ListFile[WithSource[EntityReference], Seq[WithSource[EntityReference]]] = WithSource(
    url = Files.fileInDirectory(fromUrl.url, "references-generated.xml"),
    name = "references",
    value = EntityReference
  )
  def getReferences: Caching.Parser[Seq[WithSource[EntityReference]]] = references.get

  private val unclears: ListFile[WithSource[Unclear.Value], Seq[WithSource[Unclear.Value]]] = WithSource(
    url = Files.fileInDirectory(fromUrl.url, "unclears-generated.xml"),
    name = "unclears",
    value = Unclear.element
  )
  def getUnclears: Caching.Parser[Seq[WithSource[Unclear.Value]]] = unclears.get

  override protected def index: Option[Store.Path] = Some(Seq(Index.Flat))

  // TODO ZIOify logging!
  //info(request, storePath.fold(s"--- ${Files.mkUrl(path)}")(storePath => s"YES ${storePath.mkString("")}"))

  override protected def content(
    path: Store.Path,
    extension: Option[String]
  ): Caching.Parser[Site.Response] = path.last match
    case textFacet: Document.TextFacet if extension.nonEmpty && extension.contains("xml") =>
      for
        tei: Tei <- textFacet.getTei
        content: String = renderTeiContent(tei)
      yield Site.Response(content, Tei.mimeType)

    case htmlContent: HtmlContent[?] =>
      if extension.nonEmpty && !extension.contains("html") then
        Effects.fail(s"Can't provide non-HTML content '${extension.get}' of $htmlContent")
      else
        for content: String <- renderHtmlContent(path, htmlContent.asInstanceOf[HtmlContent[Collector]])
        yield Site.Response(content, Html.mimeType)

    case _ => Effects.fail(s"Can't provide content of $path")

  override protected def navigationLinks(
    path: Store.Path,
    htmlContent: HtmlContent[Collector]
  ): Caching.Parser[Seq[ScalaXml.Element]] = htmlContent match
    case collectionAlias: Collection.Alias => collectionNavigationLinks(collectionAlias.collection)
    case collection: Collection => collectionNavigationLinks(collection)

    case documentFacet: Document.Facet =>
      val collection: Collection = documentFacet.collection
      val document: Document = documentFacet.document
      val collectionFacet: Collection.Facet[?] = documentFacet.collectionFacet
      for
        siblings: (Option[Document], Option[Document]) <- collection.siblings(documentFacet.document)
        collectionNavigationLinks: Seq[ScalaXml.Element] <- collectionNavigationLinks(collection)
        moreLinks: Seq[ScalaXml.Element] <- documentFacet match
          case _: Document.TextFacet =>
            collection.translations(document).map(translations =>
              Seq(HtmlContent.a(facsimileFacetPath(collection.facsimileFacet.of(document)))(text = Tei.facsimileSymbol)) ++ (
                for translation <- if document.isTranslation then Seq.empty else translations
                  yield HtmlContent.a(textFacetPath(collection.textFacet.of(translation)))(s"[${translation.lang}]")
                )
            )
          case _: Document.FacsimileFacet =>
            ZIO.succeed(Seq(HtmlContent.a(textFacetPath(collection.textFacet.of(document)))(text = "A")))
      yield
        val (prev: Option[Document], next: Option[Document]) = siblings

        collectionNavigationLinks ++
        prev.toSeq.map(prev => HtmlContent.a(documentFacetPath(collectionFacet.of(prev    )))("⇦"          )) ++
        Seq(                   HtmlContent.a(documentFacetPath(collectionFacet.of(document)))(document.name)) ++
        next.toSeq.map(next => HtmlContent.a(documentFacetPath(collectionFacet.of(next    )))("⇨"          )) ++
        moreLinks

    case _ => ZIO.succeed (Seq.empty)

  private def collectionNavigationLinks(collection: Collection): Caching.Parser[Seq[ScalaXml.Element]] =
    ZIO.succeed(Seq(HtmlContent.a(collectionPath(collection))(s"[${collection.names.name}]")))

  // TODO eliminate
  private def path(htmlContent: HtmlContent[Collector]): Store.Path = htmlContent match
    case facet          : Document.TextFacet      => textFacetPath(facet)
    case facet          : Document.FacsimileFacet => facsimileFacetPath(facet)
    case collection     : Collection       => collectionPath(collection)
    case collectionAlias: Collection.Alias => Seq(collectionAlias)
    case index          : Index            => Seq(index)
    case hierarchy      : Hierarchy        => store2path(hierarchy)
    case entityLists    : EntityLists      => Seq(entityLists)
    case entityList     : EntityList       => Seq(entityLists, entityList)
    case entities       : Entities         => Seq(entities)
    case entity         : Entity           => Seq(entities, entity)
    case notes          : Notes            => Seq(notes)
    case note           : Note             => Seq(notes, note)
    case _              : Reports.type     => Seq(Reports)
    case report         : Report[?]        => Seq(Reports, report)

  def entityPath(entity: Entity): Store.Path = Seq(entities, entity)

  def entityListPath(entityList: EntityList): Store.Path = Seq(entityLists, entityList)

  private def documentFacetPath(facet: Document.Facet): Store.Path = facet match
    case textFacet     : Document.TextFacet      => textFacetPath     (textFacet)
    case facsimileFacet: Document.FacsimileFacet => facsimileFacetPath(facsimileFacet)

  def textFacetPath(facet: Document.TextFacet): Store.Path =
    collectionPath(facet.collection) ++ Seq(facet)

  private def facsimileFacetPath(facet: Document.FacsimileFacet): Store.Path =
    collectionPath(facet.collection) ++ Seq(facet.collectionFacet, facet)

  def hierarchicalPath(hierarchical: Hierarchical): Store.Path = hierarchical match
    case collection: Collection => collectionPath(collection)
    case hierarchy : Hierarchy  => store2path(hierarchy)

  def collectionPath(collection: Collection): Store.Path =
    collection.alias.fold(store2path(collection))(alias => Seq(alias2collectionAlias(alias)))

  override protected def initializeResolve: Caching.Parser[Unit] = entityLists.setUp(this)

  override protected def storesPure: Seq[Store] =
    Seq(Index.Flat, Index.Tree, entityLists, entities, notes, Reports, by)

  override def findByName(name: String): Caching.Parser[Option[Store]] =
    alias2collectionAlias.get(name).fold(super.findByName(name))(result => ZIO.some(result))

  override protected def linkResolver(htmlContent: HtmlContent[Collector]): LinksResolver =
    val textFacet: Option[Document.TextFacet] = htmlContent match
      case htmlFacet: Document.TextFacet => Some(htmlFacet)
      case _ => None

    new LinksResolver:
      private val facsUrl: Option[Store.Path] = textFacet.map(textFacet =>
        facsimileFacetPath(textFacet.collection.facsimileFacet.of(textFacet.document)))

      def toUIO(parser: Caching.Parser[Option[html.a]], error: => String): UIO[Option[html.a]] =
        toTask(parser.map(a =>
          if a.isEmpty then logger.warn(error)
          a
        )).orDie

      override def resolve(url: Seq[String]): UIO[Option[html.a]] = toUIO(
        Collector.this.resolveUrl(url).map(_.map(path => HtmlContent.a(path))),
        s"did not resolve: $url"
      )

      override def findByRef(ref: String): UIO[Option[html.a]] = toUIO(
        entities.findByName(ref).map(_.map(entity => HtmlContent.a(entityPath(entity)))),
        s"did not find reference: $ref"
      )

      override def facs(pageId: String): UIO[Option[html.a]] = toUIO(
        ZIO.succeed(facsUrl.map(facsUrl => HtmlContent.a(facsUrl).setFragment(pageId))),
        "did not get facsimile: $pageId"
      )

  override protected def directoriesToWrite: Seq[Directory[?, ?, ?]] = Seq(entities, notes) ++ collections.map(_.documents)

  override protected def buildMore: Caching.Parser[Unit] = for
      _ <- Effects.effect(logger.info("Writing references."))
      allReferences: Seq[WithSource[EntityReference]] <- allWithSource[EntityReference](
        nodes => ZIO.foreach(EntityType.values.toIndexedSeq)(entityType =>
          ScalaXml.descendants(nodes, entityType.nameElement, EntityReference)).map(_.flatten) // TODO toIndexSeq?
      )
      _ <- Effects.effect(references.write(allReferences))

      _ <- Effects.effect(logger.info("Writing unclears."))
      allUnclears: Seq[WithSource[Unclear.Value]] <- allWithSource[Unclear.Value](
        nodes => ScalaXml.descendants(nodes, Unclear.element.elementName, Unclear.element)
      )
      _ <- Effects.effect(unclears.write(allUnclears))

      _ <- Effects.effect(logger.info("Verifying site."))

      errorOpts: Seq[Option[String]] <- getReferences.flatMap(ZIO.foreach(_)(value =>
        val reference: EntityReference = value.value
        val name: Element.Nodes = reference.name
        reference.ref.fold[Caching.Parser[Option[String]]](ZIO.none)(ref =>
          if ref.contains(" ") then ZIO.some(s"""Value of the ref attribute contains spaces: ref="$ref" """)
          else entities.findByName(ref).map(_
            .fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">$name< """))(named =>
              if named.entityType == reference.entityType then None
              else Some(s"${reference.entityType} reference to ${named.entityType} ${named.name}: $name [$ref]")
            )
          )
        )
      ))
      errors: Seq[String] = errorOpts.flatten
      _ <- Effects.check(errors.isEmpty, errors.mkString("\n"))

      // detect and log unresolved references
      allNotes: Seq[Note] <- notes.stores
      allEntities: Seq[Entity] <- entities.stores
      allStores: Seq[HtmlContent[Collector]] = hierarchies ++ collections ++ allNotes ++ allEntities
      _ <- ZIO.foreach_(allStores)(store => resolveLinksInHtmlContent(path(store).init, store))

      _ <- ZIO.foreach_(collections)(collection => collection.documents.stores.flatMap(ZIO.foreach_(_) { document =>
        val textFacet: Document.TextFacet = collection.textFacet.of(document)
        resolveLinksInHtmlContent(textFacetPath(textFacet).init, textFacet)
      }))
    yield ()

  def allWithSource[T](finder: ScalaXml.Nodes => Parser[Seq[T]]): Caching.Parser[Seq[WithSource[T]]] =

    def withSource(htmlContent: HtmlContent[Collector], nodes: ScalaXml.Nodes): Parser[Seq[WithSource[T]]] =
      val source: String = Files.mkUrl(Store.structureNames(path(htmlContent)))
      finder(nodes).map(_.map(new WithSource[T](source, _)))

    for
      entities: Seq[Entity] <- entities.stores
      fromEntities: Seq[Seq[WithSource[T]]] <- ZIO.foreach(entities)(entity =>
        entity.teiEntity(this).flatMap(teiEntity => withSource(entity, teiEntity.content.scalaXml)))
      fromHierarchicals: Seq[Seq[WithSource[T]]] <- ZIO.foreach(hierarchies ++ collections)(hierarchical => withSource(
        hierarchical,
        Seq(Some(hierarchical.title), hierarchical.storeAbstract, hierarchical.body).flatten.flatMap(_.content.scalaXml)
      ))
      fromDocuments: Seq[Seq[Seq[WithSource[T]]]] <- ZIO.foreach(collections)(collection =>
        collection.documents.stores.flatMap(documents => ZIO.foreach(documents)(document =>
          val textFacet: Document.TextFacet = collection.textFacet.of(document)
          textFacet.getTei.flatMap(tei => withSource(textFacet, Seq(Tei.xmlElement(tei))))
        ))
      )
    yield (fromEntities ++ fromHierarchicals ++ fromDocuments.flatten).flatten

  override protected def prettyPrintRoots: Seq[URL] = Seq(
//    fromUrl.url,  // leave the site.xml file alone :)
    entities.directoryUrl,
    Files.subdirectory(fromUrl.url, "archive") // TODO do not assume the directory name!
  )

