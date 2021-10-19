package org.opentorah.collector

import org.opentorah.html
import org.opentorah.site.{HtmlContent, Site, SiteCommon}
import org.opentorah.store.{Alias, Directory, ListFile, Path, Store, WithSource}
import org.opentorah.tei.{EntityReference, EntityType, LinksResolver, Tei, Unclear, Entity as TeiEntity}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Caching, Element, Parser, ScalaXml}
import zio.{UIO, ZIO}
import java.net.URL

// TODO retrieve TEI(?) references from notes.
final class Collector(
  fromUrl: Element.FromUrl,
  common: SiteCommon,
  val entities: Entities,
  val entityLists: EntityLists,
  val notes: Notes,
  val by: ByHierarchy,
  val aliases: Seq[Alias]
) extends Site[Collector](
  fromUrl,
  common
):
  def collectionPaths: Caching.Parser[Seq[Path]] = Caching.getCached(
    key = "collectionPaths",
    load = by.getPaths(include = _.isInstanceOf[Collection], stop = _.isInstanceOf[Collection])
  )

  def entityPath(entity: Entity): Path = Seq(entities, entity)

  def entityListPath(entityList: EntityList): Path = Seq(entityLists, entityList)

  private def findEntityByName[T](name: String, action: Option[Entity] => T): Caching.Parser[T] =
    entities.findByName(name).map(action)

  def collectionPath(path: Path): Path =
    val collectionIndex: Int = path.indexWhere(_.isInstanceOf[Collection])
    require(collectionIndex > 0)
    path.take(collectionIndex+1)

  // TODO get rid of the unsafeRun() here, shortenPath() - and a() - need to become Caching.Parsers...
  private def store2alias: Map[Store, Alias] = Caching.unsafeRun(caching, Caching.getCached(
    key = "store2alias",
    load = ZIO.foreach(aliases)((alias: Alias) =>
      for toPath: Path <- resolve(alias.to) yield toPath.last -> alias
    ).map(_.toMap)
  ))

  override protected def shortenPath(path: Path): Path =
    def useAliases(path: Path): Path =
      val aliasedIndex: Int = path.indexWhere(store2alias.contains)
      if aliasedIndex < 0 then path else store2alias(path(aliasedIndex)) +: path.drop(aliasedIndex+1)

    def skipTextFacet(path: Path): Path =
      if
        path.length >= 2 &&
        path.last.isInstanceOf[TextFacet] &&
        path.init.last.isInstanceOf[Collection.CollectionTextFacet]
      then path.init.init :+ path.last else path

    useAliases(skipTextFacet(path))

  override protected def index: Option[Path] = Some(Seq(Index.Flat))

  override protected def storesPure: Seq[Store] =
    Seq(Index.Flat, Index.Tree, entityLists, entities, notes, Reports, by) ++ aliases

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

  // TODO ZIOify logging!
  //info(request, storePath.fold(s"--- ${Files.mkUrl(path)}")(storePath => s"YES ${storePath.mkString("")}"))

  override protected def content(
    path: Path,
    extension: Option[String]
  ): Caching.Parser[Site.Response] = path.last match
    case textFacet: TextFacet if extension.nonEmpty && extension.contains("xml") =>
      for
        tei: Tei <- textFacet.getTei
        content: String = renderTeiContent(tei)
      yield Site.Response(content, Tei.mimeType)

    case htmlContent: HtmlContent[?] =>
      if extension.nonEmpty && !extension.contains("html") then
        Effects.fail(s"Can't provide non-HTML content '${extension.get}' of $htmlContent")
      else
        for content: String <- renderHtmlContent(path, htmlContent.asInstanceOf[HtmlContent[Collector]])
        yield Site.Response(content, html.Html.mimeType)

    case _ => Effects.fail(s"Can't provide content of $path")

  override protected def initializeResolve: Caching.Parser[Unit] = entityLists.setUp(this)

  override protected def linkResolver(path: Path, htmlContent: HtmlContent[Collector]): LinksResolver =
    val facsUrl: Option[Path] = htmlContent match
      case textFacet: TextFacet => Some(textFacet.document.facetPath(collectionPath(path), textFacet.collection.facsimileFacet))
      case _ => None

    new LinksResolver:
      def toUIO(
        pathFinder: Caching.Parser[Option[Path]],
        error: => String,
        modifier: html.a => html.a = identity,
      ): UIO[Option[html.a]] = toTask(pathFinder.map(pathOpt =>
        if pathOpt.isEmpty then logger.warn(error)
        pathOpt.map((path: Path) => modifier(a(path)))
      )).orDie

      override def resolve(url: Seq[String]): UIO[Option[html.a]] = toUIO(
        pathFinder = Collector.this.resolveUrl(url),
        error = s"did not resolve: $url"
      )

      override def findByRef(ref: String): UIO[Option[html.a]] = toUIO(
        pathFinder = findEntityByName(ref, _.map(entityPath)),
        error = s"did not find reference: $ref"
      )

      override def facs(pageId: String): UIO[Option[html.a]] = toUIO(
        pathFinder = ZIO.succeed(facsUrl),
        error = s"did not get facsimile: $pageId",
        modifier = _.setFragment(pageId)
      )

  override protected def directoriesToWrite: Caching.Parser[Seq[Directory[?, ?, ?]]] =
    for collectionPaths: Seq[Path] <- collectionPaths
    yield Seq(entities, notes) ++ collectionPaths.map(_.last.asInstanceOf[Collection].documents)

  private def notePaths: Caching.Parser[Seq[Path]] =
    notes.getPaths(include = _.isInstanceOf[Note], stop = _.isInstanceOf[Note])

  private def entityPaths: Caching.Parser[Seq[Path]] =
    entities.getPaths(include = _.isInstanceOf[Entity], stop = _.isInstanceOf[Entity])

  private def hierarchyPaths: Caching.Parser[Seq[Path]] =
    by.getPaths(include = _.isInstanceOf[Hierarchical], stop = _.isInstanceOf[Collection])

  // TODO must include translations!
  private def textFacetPaths: Caching.Parser[Seq[Path]] =
    by.getPaths(include = _.isInstanceOf[TextFacet],
      stop = store => store.isInstanceOf[TextFacet] || store.isInstanceOf[Collection.CollectionFacsimileFacet])

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
        else findEntityByName(ref, _
          .fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">$name< """))(named =>
            if named.entityType == reference.entityType then None
            else Some(s"${reference.entityType} reference to ${named.entityType} ${named.name}: ${name.scalaXml} [$ref]")
          )
        )
      )
    ))
    errors: Seq[String] = errorOpts.flatten
    _ <- Effects.check(errors.isEmpty, errors.mkString("\n"))

    // detect and log unresolved references
    _ <- verifyLinks(notePaths)
    _ <- verifyLinks(entityPaths)
    _ <- verifyLinks(hierarchyPaths)
    _ <- verifyLinks(textFacetPaths)
  yield ()

  private def verifyLinks(getter: Caching.Parser[Seq[Path]]): Caching.Parser[Unit] = for
    paths: Seq[Path] <- getter
    _ <- ZIO.foreach_(paths)((path: Path) =>
      // TODO separate verification from actual resolution
      resolveLinksInHtmlContent(path, path.last.asInstanceOf[HtmlContent[Collector]])
    )
  yield ()

  private def allWithSource[T](finder: ScalaXml.Nodes => Parser[Seq[T]]): Caching.Parser[Seq[WithSource[T]]] =
    def forPaths[S, R](
      getter: Caching.Parser[Seq[Path]],
      retriever: S => Caching.Parser[R],
      extractor: R => ScalaXml.Nodes
    ): Caching.Parser[Seq[WithSource[T]]] = for
      paths: Seq[Path] <- getter
      result: Seq[Seq[WithSource[T]]] <- ZIO.foreach(paths)((path: Path) =>
        for
          from: R <- retriever(path.last.asInstanceOf[S])
          source: String = Files.mkUrl(Path.structureNames(shortenPath(path)))
          nodes: ScalaXml.Nodes = extractor(from)
          found: Seq[T] <- finder(nodes)
        yield found.map(new WithSource[T](source, _))
      )
    yield result.flatten

    for
      fromEntities: Seq[WithSource[T]] <- forPaths[Entity, TeiEntity](
        getter = entityPaths,
        retriever = _.getTei(this),
        extractor = _.content.scalaXml
      )

      fromHierarchicals: Seq[WithSource[T]] <- forPaths[Hierarchical, Hierarchical](
        getter = hierarchyPaths,
        retriever = ZIO.succeed,
        extractor = (hierarchical: Hierarchical) =>
          Seq(Some(hierarchical.title), hierarchical.storeAbstract, hierarchical.body)
            .flatten.flatMap(_.content.scalaXml)
      )

      fromDocuments: Seq[WithSource[T]] <- forPaths[TextFacet, Tei](
        getter = textFacetPaths,
        retriever = _.getTei,
        extractor = tei => Seq(Tei.xmlElement(tei))
      )
    yield fromEntities ++ fromHierarchicals ++ fromDocuments

  override protected def prettyPrintTei: Caching.Parser[Seq[URL]] = for
    entityPaths <- entityPaths
    textFacetPaths <- textFacetPaths
  yield
    entityPaths.map(path => entities.fileUrl(path.last.asInstanceOf[Entity].name)) ++
    textFacetPaths.map(_.last.asInstanceOf[TextFacet])
      .map(textFacet => textFacet.collection.documents.fileUrl(textFacet.document))

  override protected def prettyPrintStores: Caching.Parser[Seq[URL]] = for
    hierarchyPaths <- hierarchyPaths
  yield
    //Seq(fromUrl.url) ++ // leave the site.xml file alone :)
    hierarchyPaths.map(_.last.asInstanceOf[Element.FromUrl.With].fromUrl).filterNot(_.inline).map(_.url)
