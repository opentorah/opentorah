package org.opentorah.collector

import java.io.File
import org.opentorah.store.{Entities, EntityHolder, Path, Store, WithPath}
import org.opentorah.tei.{Entity, EntityReference, Publisher, SourceDesc, Tei}
import org.opentorah.util.Files
import org.opentorah.xml.LinkResolver
import org.slf4j.{Logger, LoggerFactory}
import scala.xml.Node

final class Site(
  val store: Store,
  val siteParameters: SiteParameters
) {

  private def withPath[R](values: Store => Seq[R]): Seq[WithPath[R]] =
    Site.withPath(Path.empty, values, store)

  val references: Seq[WithPath[EntityReference]] = withPath[EntityReference](values = references(_))

  private def references(store: Store): Seq[EntityReference] = store match {
    case _: Entities =>
      Seq.empty

    case entityHolder: EntityHolder =>
      EntityReference.from(entityHolder.entity.content)

    case _: Document =>
      Seq.empty

    case teiHolder: TeiHolder =>
      val lookInto: Seq[Node] =
        teiHolder.tei.getAbstract.getOrElse(Seq.empty) ++
        teiHolder.tei.correspDesc.map(_.xml).getOrElse(Seq.empty) ++
        teiHolder.tei.body.xml

      teiHolder.tei.titleStmt.references ++ EntityReference.from(lookInto)

    case fromElement: Store.FromElement =>
      val lookInto: Seq[Node] =
        fromElement.title.map(_.xml).getOrElse(Seq.empty) ++
        fromElement.storeAbstract.map(_.xml).getOrElse(Seq.empty) ++
        fromElement.body.map(_.xml).getOrElse(Seq.empty)

      EntityReference.from(lookInto)
  }

  private val collections: Seq[WithPath[Collection]] = withPath[Collection](values = {
    case collection: Collection => Seq(collection)
    case _ => Seq.empty
  })

  def publishedCollections: Seq[WithPath[Collection]] = collections.filterNot(collection =>
    Site.unpublishedCollections.contains(Hierarchy.fileName(collection.value)))

  def findCollectionByName(collectionName: String): Option[WithPath[Collection]] =
    collections.find(collection => Hierarchy.fileName(collection.value) == collectionName)

  val entities: Seq[Entity] = store.entities.get.by.get.stores.map(_.entity)

  def findByRef(fileName: String): Option[Entity] =  store.entities.get.findByRef(fileName)

  def resolver(facsUrl: Seq[String]): LinkResolver = new LinkResolver {

    override def resolve(url: Seq[String]): Option[LinkResolver.Resolved] =
      new RootSiteObject(Site.this).resolve(url).fold[Option[LinkResolver.Resolved]] {
        Site.logger.warn(s"did not resolve: $url")
        None
      } { siteFile: SiteFile => Some(LinkResolver.Resolved(
        url = siteFile.url,
        role = siteFile match {
          case htmlFile: SiteFile => Some(htmlFile.viewer.name)
          case _ => None
        }
      ))}

    override def findByRef(ref: String): Option[LinkResolver.Resolved] =
      Site.this.findByRef(ref).map { entity: Entity => Some(LinkResolver.Resolved(
        url = EntityObject.teiWrapperUrl(entity),
        role = Some(Viewer.Names.name)
      ))}.getOrElse {
        Site.logger.warn(s"did not find reference: $ref")
        None
      }

    override def facs: LinkResolver.Resolved = LinkResolver.Resolved(
      url = facsUrl,
      role = Some(Viewer.Facsimile.name)
    )
  }
}

object Site {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val facsimileBucket: String = "https://storage.googleapis.com/facsimiles.alter-rebbe.org/facsimiles/"

  private val unpublishedCollections: Set[String] = Set("niab5", "niab19", "rnb203")

  // TODO with images on a separate website (facsimiles.alter-rebbe.org), this has to be re-worked...
  //  private def checkPages(): Unit = {
  //    val imageNames: Set[String] = Util.filesWithExtensions(directory = layout.facsimiles(directory), ".jpg").toSet
  //    imageNames.foreach(name => pageType(name, isPresent = true))
  //    val usedImages: Set[String] = pages.filter(_.isPresent).map(_.name).toSet
  //    val orphanImages: Seq[String] = (imageNames -- usedImages).toSeq.sorted
  //    val missingImages: Seq[String] = (usedImages -- imageNames).toSeq.sorted
  //    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
  //    if (missingImages.nonEmpty) throw new IllegalArgumentException(s"Missing images: $missingImages")
  //  }

  private def withPath[R](
    path: Path,
    values: Store => Seq[R],
    store: Store
  ): Seq[WithPath[R]] = {
    val fromStore: Seq[WithPath[R]] =
      values(store).map(WithPath[R](path, _))

    val fromEntities: Seq[WithPath[R]] = store.entities.toSeq.flatMap(entities =>
      withPath[R](path :+ entities.selector.bind(entities), values, entities))

    val fromBy: Seq[WithPath[R]] = store.by.toSeq.flatMap(by =>
      by.stores.flatMap(store => withPath[R](path :+ by.selector.bind(store), values, store)))

    fromEntities ++ fromStore ++ fromBy
  }

  def write(
    directory: File,
    site: Site
  ): Unit = {
    writeHtmlFile(new IndexObject(site), directory)
    writeHtmlFile(new TreeIndexObject(site), directory)
    writeHtmlFile(new NamesObject(site), directory)

    Files.deleteFiles(new File(directory, EntityObject.directoryName))
    for (entity <- site.entities) writeHtmlFile(new EntityObject(site, entity), directory)

    Files.deleteFiles(new File(directory, Hierarchy.directoryName))
    val stores: Seq[WithPath[Store]] = site.withPath[Store](values = {
      case _: Collection | _: Document | _: Entities | _: EntityHolder | _: TeiHolder => Seq.empty
      case store => Seq(store)
    })
    for (store <- stores) writeHtmlFile(new HierarchyObject(site, store.path, store.value), directory)

    Files.deleteFiles(new File(directory, CollectionObject.directoryName))
    for (collection <- site.collections)
      writeHtmlFile(new CollectionObject(site, collection), directory)

    for {
      collection <- site.collections
      document <- collection.value.documents
      teiHolder: TeiHolder <- document.teiHolders
    } {
      val documentObject = new DocumentObject(site, collection, document, teiHolder)
      writeHtmlFile(documentObject, directory)
      writeSiteFile(documentObject, documentObject.facsFile, directory)
    }

    Files.deleteFiles(new File(directory, ReportsObject.directoryName))
    writeHtmlFile(new ReportsObject(site), directory)
    writeHtmlFile(new MisnamedEntitiesReport(site), directory)
    writeHtmlFile(new NoRefsReport(site), directory)
  }

  private final def writeHtmlFile(siteObject: SiteObject, directory: File): Unit =
    writeSiteFile(siteObject, siteObject.htmlFile, directory)

  private final def writeSiteFile(siteObject: SiteObject, siteFile: SiteFile, directory: File): Unit =
    Files.write(Files.file(directory, siteFile.url), siteObject.content(siteFile))

  val addPublicationStatement: Tei.Transformer = Tei.addPublicationStatement(
    publisher = new Publisher.Value(<ptr xmlns={Tei.namespace.uri} target="www.alter-rebbe.org"/>),
    status = "free",
    licenseName = "Creative Commons Attribution 4.0 International License",
    licenseUrl = "http://creativecommons.org/licenses/by/4.0/"
  )

  val addSourceDesc: Tei.Transformer = Tei.addSourceDesc(new SourceDesc.Value(<p>Facsimile</p>))
}
