package org.opentorah.collector

import org.opentorah.html.{A, Html}
import org.opentorah.site.{LinksResolver, Site, SiteCommon, SiteService}
import org.opentorah.store.{Alias, Context, Directory, ListFile, Path, Store, WithSource}
import org.opentorah.tei.{EntityReference, EntityType, Tei, Unclear, Entity as TeiEntity}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{FromUrl, Nodes, Parsable, Parser, Unparser}
import zio.{UIO, ZIO}
import java.net.URL

final class Collector(
  fromUrl: FromUrl,
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
  def collectionPaths: Parser[Seq[Path]] = Parser.getCached(
    key = "collectionPaths",
    load = by.getPaths(include = _.isInstanceOf[Collection], stop = _.isInstanceOf[Collection])
  )

  override def path(store: Store): Path = store match
    case entity: Entity => Seq(entities, entity)
    case entityList: EntityList => Seq(entityLists, entityList)

  override def viewer(store: Store): String = store match
    case _: Collection | _: Hierarchical | _: Hierarchy => viewerDefault
    case _: TextFacet => "textViewer"
    case _: FacsimileFacet => "facsimileViewer"
    case _: Entities | _: EntityLists | _: EntityList | _: Entity | _: Reports.type => "apparatusViewer"
    case _ => viewerDefault

  override def viewerDefault: String = "hierarchyViewer"

  override def style(store: Store): String = store match
    case _: Collection => "wide"
    case _ => "main"

  override def wrapperCssClass(store: Store): String = store match
    case _: TextFacet => "textWrapper"
    case _: FacsimileFacet => "facsimileWrapper"
    case _ => null

  private def findEntityByName[T](name: String, action: Option[Entity] => T): Parser[T] =
    entities.findByName(name).map(action)

  private def store2alias: Parser[Map[Store, Alias]] = Parser.getCached(
    key = "store2alias",
    load = ZIO.foreach(aliases)((alias: Alias) =>
      for toPath: Path <- resolve(alias.to) yield toPath.last -> alias
    ).map(_.toMap)
  )

  override def pathShortener: Parser[Path.Shortener] =
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
  
  def enityReferencesUrl: URL = Files.subdirectory(fromUrl.url, "names-references")

  private var references: Seq[WithSource[EntityReference]] = Seq.empty

  // TODO move into the report
  private val noRefs: ListFile[WithSource[EntityReference], Seq[WithSource[EntityReference]]] = WithSource(
    url = Files.fileInDirectory(fromUrl.url, "noRefs-generated.xml"),
    name = "references",
    value = EntityReference
  )
  def getNoRefs: Parser[Seq[WithSource[EntityReference]]] = noRefs.get

  // TODO move into the report
  private val unclears: ListFile[WithSource[Unclear.Value], Seq[WithSource[Unclear.Value]]] = WithSource(
    url = Files.fileInDirectory(fromUrl.url, "unclears-generated.xml"),
    name = "unclears",
    value = Unclear.element
  )
  def getUnclears: Parser[Seq[WithSource[Unclear.Value]]] = unclears.get

  // TODO ZIOify logging!
  //info(request, storePath.fold(s"--- ${Files.mkUrl(path)}")(storePath => s"YES ${storePath.mkString("")}"))

  override protected def content(
    path: Path,
    extension: Option[String]
  ): Parser[Site.Response] = path.last match
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
        yield Site.Response(content, Html.mimeType)

  override protected def initializeResolve: Parser[Unit] = entityLists.setUp(this)

  override protected def linkResolver(path: Path): LinksResolver =
    val facsUrl: Option[Path] = path.last match
      case textFacet: TextFacet => Some(textFacet.document.facetPath(Collector.collectionPath(path), textFacet.collection.facsimileFacet))
      case _ => None

    new LinksResolver:
      def toUIO(
        pathFinder: Parser[Option[Path]],
        error: => String,
        modifier: A => A = identity,
      ): UIO[Option[A]] = toTask(
        for
          pathOpt: Option[Path] <- pathFinder
          _ <- if pathOpt.isEmpty then ZIO.succeed(logger.warn(error)) else ZIO.unit
          result: Option[A] <-
            if pathOpt.isEmpty
            then ZIO.none
            else for a: A <- a(pathOpt.get) yield Some(modifier(a))
        yield
          result
      ).orDie

      override def resolve(url: Seq[String]): UIO[Option[A]] = toUIO(
        pathFinder = Collector.this.resolveUrl(url),
        error = s"did not resolve: $url"
      )

      override def findByRef(ref: String): UIO[Option[A]] = toUIO(
        pathFinder = findEntityByName(ref, _.map(Collector.this.path)),
        error = s"did not find reference: $ref"
      )

      override def facs(pageId: String): UIO[Option[A]] = toUIO(
        pathFinder = ZIO.succeed(facsUrl),
        error = s"did not get facsimile: $pageId",
        modifier = _.setFragment(pageId)
      )

  override protected def directoriesToWrite: Parser[Seq[Directory[?, ?, ?]]] =
    for collectionPaths: Seq[Path] <- collectionPaths
    yield Seq(entities, notes) ++ collectionPaths.map(_.last.asInstanceOf[Collection].documents)

  private def notePaths: Parser[Seq[Path]] =
    notes.getPaths(include = _.isInstanceOf[Note], stop = _.isInstanceOf[Note])

  private def entityPaths: Parser[Seq[Path]] =
    entities.getPaths(include = _.isInstanceOf[Entity], stop = _.isInstanceOf[Entity])

  private def hierarchyPaths: Parser[Seq[Path]] =
    by.getPaths(include = _.isInstanceOf[Hierarchical], stop = _.isInstanceOf[Collection])

  // TODO must include translations!
  private def textFacetPaths: Parser[Seq[Path]] = by.getPaths(include = _.isInstanceOf[TextFacet], stop = {
    case _: TextFacet => true
    case collectionFacet: CollectionFacet => !collectionFacet.isText
    case _ => false
  })

  override protected def innerBuild: Parser[Unit] = for
    _ <- writeReferences
    _ <- writeUnclears
  yield ()

  private def writeReferences: Parser[Unit] =
    logger.info("Writing references.")
    for
      allReferences: Seq[WithSource[EntityReference]] <- allWithSource[EntityReference](nodes =>
        ZIO.foreach(EntityType.values.toIndexedSeq)(entityType =>
            EntityReference.descendants(nodes, entityType.nameElement)
        )
          .map(_.flatten) // TODO toIndexSeq?
      )
      _ <- Effects.effect({references = allReferences})
      // TODO move into the report
      _ <- Effects.effect(noRefs.write(allReferences
        .filter(_.value.ref.isEmpty)
        .sortBy(reference => reference.value.name.toString.toLowerCase)
      ))
      // TODO move into the Entities
      allEntities: Seq[Entity] <- entities.stores
      _ <- ZIO.foreachDiscard(allEntities)((entity: Entity) => Effects.effect(entity.writeReferences(allReferences, this)))
    yield ()

  private def writeUnclears: Parser[Unit] =
    logger.info("Writing unclears.")
    for
      allUnclears: Seq[WithSource[Unclear.Value]] <- allWithSource[Unclear.Value](
        nodes => Unclear.element.descendants(nodes, Unclear.element.elementName)
      )
      _ <- Effects.effect(unclears.write(allUnclears.sortBy(_.source)))
    yield ()

  // TODO retrieve TEI(?) references from notes.
  private def allWithSource[T](finder: Nodes => Parser[Seq[T]]): Parser[Seq[WithSource[T]]] =
    def forPaths[S, R](
      getter: Parser[Seq[Path]],
      retriever: S => Parser[R],
      extractor: R => Nodes
    ): Parser[Seq[WithSource[T]]] = for
      paths: Seq[Path] <- getter
      result: Seq[Seq[WithSource[T]]] <- ZIO.foreach(paths)((path: Path) =>
        for
          from: R <- retriever(Path.last[S](path))
          pathShortener: Path.Shortener <- pathShortener
          source: String = Files.mkUrl(Path.structureNames(pathShortener(path)))
          nodes: Nodes = extractor(from)
          found: Seq[T] <- finder(nodes)
        yield found.map(new WithSource[T](source, _))
      )
    yield result.flatten

    for
      fromEntities: Seq[WithSource[T]] <- forPaths[Entity, TeiEntity](
        getter = entityPaths,
        retriever = _.getTei(this),
        extractor = _.content
      )

      fromHierarchicals: Seq[WithSource[T]] <- forPaths[Hierarchical, Hierarchical](
        getter = hierarchyPaths,
        retriever = ZIO.succeed,
        extractor = (hierarchical: Hierarchical) =>
          Seq(Some(hierarchical.title), hierarchical.description, hierarchical.body)
            .flatten.flatMap(_.content)
      )

      fromDocuments: Seq[WithSource[T]] <- forPaths[TextFacet, Tei](
        getter = textFacetPaths,
        retriever = _.getTei,
        extractor = tei => Seq(Tei.xmlElement(tei))
      )
    yield fromEntities ++ fromHierarchicals ++ fromDocuments

  override protected def verify: Parser[Unit] =
    logger.info("Verifying site.")
    for
      errorOpts: Seq[Option[String]] <- ZIO.foreach(references)(value =>
        val reference: EntityReference = value.value
        val name: Nodes = reference.name
        reference.ref.fold[Parser[Option[String]]](ZIO.none)(ref =>
          if ref.contains(" ") then ZIO.some(s"""Value of the ref attribute contains spaces: ref="$ref" """)
          else findEntityByName(ref, _
            .fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${Nodes.toString(name)}< """))(named =>
              if named.entityType == reference.entityType then None
              else Some(s"${reference.entityType} reference to ${named.entityType} ${named.name}: ${Nodes.toString(name)} [$ref]")
            )
          )
        )
      )
      errors: Seq[String] = errorOpts.flatten
      _ <- Effects.check(errors.isEmpty, errors.mkString("\n"))

      // detect and log unresolved references
      _ <- verifyLinks(notePaths)
      _ <- verifyLinks(entityPaths)
      _ <- verifyLinks(hierarchyPaths)
      _ <- verifyLinks(textFacetPaths)
    yield ()

  private def verifyLinks(getter: Parser[Seq[Path]]): Parser[Unit] = for
    paths: Seq[Path] <- getter
    // TODO separate verification from actual resolution
    _ <- ZIO.foreachDiscard(paths)(resolveLinks)
  yield ()

  override protected def prettyPrintTei: Parser[Seq[URL]] = for
    entityPaths <- entityPaths
    textFacetPaths <- textFacetPaths
  yield
    entityPaths.map(path => entities.fileUrl(Path.last[Entity](path).name)) ++
    textFacetPaths.map(_.last.asInstanceOf[TextFacet])
      .map(textFacet => textFacet.collection.documents.fileUrl(textFacet.document))

  override protected def prettyPrintStores: Parser[Seq[URL]] = for
    hierarchyPaths <- hierarchyPaths
  yield
    //Seq(fromUrl.url) ++ // leave the site.xml file alone :)
    hierarchyPaths.map(_.last.asInstanceOf[FromUrl.With].fromUrl).filterNot(_.inline).map(_.url)

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
      fromUrl: FromUrl <- FromUrl.get
      common: SiteCommon <- SiteCommon.required()
      entities: Entities <- Entities.required()
      entityLists: EntityLists <- EntityLists.required()
      notes: Notes <- Notes.required()
      // TODO do not follow the includes; instead, cache the parsed store from a stub that has the URL?
      by: ByHierarchy <- ByHierarchy.required()
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
