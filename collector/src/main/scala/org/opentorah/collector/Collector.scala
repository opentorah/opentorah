package org.opentorah.collector

import org.opentorah.html
import org.opentorah.site.{Site, SiteCommon, SiteService}
import org.opentorah.store.{Alias, Directory, Context, ListFile, Path, Store, WithSource}
import org.opentorah.tei.{EntityReference, EntityType, LinksResolver, Tei, Unclear, Entity as TeiEntity}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Caching, Element, Parsable, Parser, ScalaXml, Unparser}
import zio.{UIO, ZIO}
import java.net.URL

final class Collector(
  fromUrl: Element.FromUrl,
  common: SiteCommon,
  val entities: Entities,
  val entityLists: EntityLists,
  val notes: Notes,
  val by: ByHierarchy,
  val aliases: Seq[Alias]
) extends Site(
  fromUrl,
  common
):
  def collectionPaths: Caching.Parser[Seq[Path]] = Caching.getCached(
    key = "collectionPaths",
    load = by.getPaths(include = _.isInstanceOf[Collection], stop = _.isInstanceOf[Collection])
  )

  override def path(store: Store): Path = store match
    case entity: Entity => Seq(entities, entity)
    case entityList: EntityList => Seq(entityLists, entityList)

  private def findEntityByName[T](name: String, action: Option[Entity] => T): Caching.Parser[T] =
    entities.findByName(name).map(action)

  private def store2alias: Caching.Parser[Map[Store, Alias]] = Caching.getCached(
    key = "store2alias",
    load = ZIO.foreach(aliases)((alias: Alias) =>
      for toPath: Path <- resolve(alias.to) yield toPath.last -> alias
    ).map(_.toMap)
  )

  override def pathShortener: Caching.Parser[Path.Shortener] =
    def useAliases(path: Path, store2alias: Map[Store, Alias]): Path =
      val aliasedIndex: Int = path.indexWhere(store2alias.contains)
      if aliasedIndex < 0 then path else store2alias(path(aliasedIndex)) +: path.drop(aliasedIndex+1)

    def skipTextFacet(path: Path): Path = if
      (path.length >= 2) &&
      path.last.isInstanceOf[TextFacet] &&
      path.init.last.isInstanceOf[CollectionFacet]
    then path.init.init :+ path.last else path

    for store2alias <- store2alias
    yield (path: Path) => useAliases(skipTextFacet(path), store2alias)

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
        content: String = renderTei(tei)
      yield Site.Response(content, Tei.mimeType)

    case store: Store =>
      if extension.nonEmpty && !extension.contains("html") then
        Effects.fail(s"Can't provide non-HTML content '${extension.get}' of $store")
      else
        for content: String <- render(path)
        yield Site.Response(content, html.Html.mimeType)
  
  override protected def initializeResolve: Caching.Parser[Unit] = entityLists.setUp(this)

  override protected def linkResolver(
    path: Path,
    pathShortener: Path.Shortener
  ): LinksResolver =
    val facsUrl: Option[Path] = path.last match
      case textFacet: TextFacet => Some(textFacet.document.facetPath(Collector.collectionPath(path), textFacet.collection.facsimileFacet))
      case _ => None

    new LinksResolver:
      def toUIO(
        pathFinder: Caching.Parser[Option[Path]],
        error: => String,
        modifier: html.a => html.a = identity,
      ): UIO[Option[html.a]] = toTask(pathFinder.map(pathOpt =>
        if pathOpt.isEmpty then logger.warn(error)
        pathOpt.map((path: Path) => modifier(Path.a(path, pathShortener)))
      )).orDie

      override def resolve(url: Seq[String]): UIO[Option[html.a]] = toUIO(
        pathFinder = Collector.this.resolveUrl(url),
        error = s"did not resolve: $url"
      )

      override def findByRef(ref: String): UIO[Option[html.a]] = toUIO(
        pathFinder = findEntityByName(ref, _.map(Collector.this.path)),
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
  private def textFacetPaths: Caching.Parser[Seq[Path]] = by.getPaths(include = _.isInstanceOf[TextFacet], stop = {
    case _: TextFacet => true
    case collectionFacet: CollectionFacet => !collectionFacet.isText
    case _ => false
  })

  override protected def innerBuild: Caching.Parser[Unit] = for
    _ <- writeReferences
    _ <- writeUnclears
  yield ()

  private def writeReferences: Caching.Parser[Unit] =
    logger.info("Writing references.")
    for
      allReferences: Seq[WithSource[EntityReference]] <- allWithSource[EntityReference](
        nodes => ZIO.foreach(EntityType.values.toIndexedSeq)(entityType =>
          ScalaXml.descendants(nodes, entityType.nameElement, EntityReference)).map(_.flatten) // TODO toIndexSeq?
      )
      _ <- Effects.effect(references.write(allReferences))
    yield ()

  private def writeUnclears: Caching.Parser[Unit] =
    logger.info("Writing unclears.")
    for
      allUnclears: Seq[WithSource[Unclear.Value]] <- allWithSource[Unclear.Value](
        nodes => ScalaXml.descendants(nodes, Unclear.element.elementName, Unclear.element)
      )
      _ <- Effects.effect(unclears.write(allUnclears))
    yield ()

  // TODO retrieve TEI(?) references from notes.
  private def allWithSource[T](finder: ScalaXml.Nodes => Parser[Seq[T]]): Caching.Parser[Seq[WithSource[T]]] =
    def forPaths[S, R](
      getter: Caching.Parser[Seq[Path]],
      retriever: S => Caching.Parser[R],
      extractor: R => ScalaXml.Nodes
    ): Caching.Parser[Seq[WithSource[T]]] = for
      paths: Seq[Path] <- getter
      result: Seq[Seq[WithSource[T]]] <- ZIO.foreach(paths)((path: Path) =>
        for
          from: R <- retriever(Path.last[S](path))
          pathShortener: Path.Shortener <- pathShortener
          source: String = Files.mkUrl(Path.structureNames(pathShortener(path)))
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
          Seq(Some(hierarchical.title), hierarchical.description, hierarchical.body)
            .flatten.flatMap(_.content.scalaXml)
      )

      fromDocuments: Seq[WithSource[T]] <- forPaths[TextFacet, Tei](
        getter = textFacetPaths,
        retriever = _.getTei,
        extractor = tei => Seq(Tei.xmlElement(tei))
      )
    yield fromEntities ++ fromHierarchicals ++ fromDocuments

  override protected def verify: Caching.Parser[Unit] =
    logger.info("Verifying site.")
    for
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
    // TODO separate verification from actual resolution
    _ <- ZIO.foreach_(paths)(resolveLinks)
  yield ()

  override protected def prettyPrintTei: Caching.Parser[Seq[URL]] = for
    entityPaths <- entityPaths
    textFacetPaths <- textFacetPaths
  yield
    entityPaths.map(path => entities.fileUrl(Path.last[Entity](path).name)) ++
    textFacetPaths.map(_.last.asInstanceOf[TextFacet])
      .map(textFacet => textFacet.collection.documents.fileUrl(textFacet.document))

  override protected def prettyPrintStores: Caching.Parser[Seq[URL]] = for
    hierarchyPaths <- hierarchyPaths
  yield
    //Seq(fromUrl.url) ++ // leave the site.xml file alone :)
    hierarchyPaths.map(_.last.asInstanceOf[Element.FromUrl.With].fromUrl).filterNot(_.inline).map(_.url)

object Collector extends SiteService[Collector]:
  // TODO I sacrificed type-safety: HtmlContent lost type parameter S <: Site[S] and got merged into Store;
  // this was done so that I don't have to add this parameter to Store and friends...
  // Lift appropriate methods into HtmlContext and minimize the use of this...
  def get(context: Context): Collector = context.asInstanceOf[Collector]

  def collectionPath(path: Path): Path = Path.takeTo(path, classOf[Collection])

  override def projectId: String = "alter-rebbe-2"

  override def bucketName: String = "store.alter-rebbe.org"

  override def contentParsable: Parsable[Collector] = new Parsable[Collector]:
    override def parser: Parser[Collector] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      common: SiteCommon <- SiteCommon.required()
      entities: Entities <- Entities.required()
      entityLists: EntityLists <- EntityLists.required()
      notes: Notes <- Notes.required()
      // TODO do not follow redirects; instead, cache the parsed store from a stub that has the URL?
      by: ByHierarchy <- ByHierarchy.followRedirects.required()
      aliases: Seq[Alias] <- Alias.seq()
    yield Collector(
      fromUrl,
      common,
      entities,
      entityLists,
      notes,
      by,
      aliases
    )

    override def unparser: Unparser[Collector] = Unparser.concat[Collector](
      SiteCommon.required(_.common),
      Entities.required(_.entities),
      EntityLists.required(_.entityLists),
      Notes.required(_.notes),
      ByHierarchy.required(_.by),
      Alias.seq(_.aliases)
    )
