package org.opentorah.collector

import java.io.File
import org.opentorah.store.{Entities, EntityHolder, Path, Store, WithPath}
import org.opentorah.tei.{Entity, Publisher, SourceDesc, Tei}
import org.opentorah.util.Files
import org.opentorah.xml.{LinkResolver, PrettyPrinter}
import org.slf4j.{Logger, LoggerFactory}
import java.net.URL
import scala.xml.Elem

final class Site(fromUrl: URL) {
  val store: Store = Store.read(Files.fileInDirectory(Files.subdirectory(fromUrl, "store"), "store.xml"))

  val siteParameters: SiteParameters = Site.mkSiteParameters

  private def withPath[R](values: Store => Seq[R]): Seq[WithPath[R]] =
    Store.withPath(Path.empty, values, store)

  val references: References = References(store)

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
      rootSiteObject.resolve(url).fold[Option[LinkResolver.Resolved]] {
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

  private val rootSiteObject: RootSiteObject = new RootSiteObject(this)

  def write(doWrite: Boolean): Unit = {
    val directory: File = Files.url2file(fromUrl)

    def writeHtmlFile(siteObject: SiteObjectWithFile, directory: File): Unit =
      writeSiteFile(siteObject.htmlFile, directory)

    def writeSiteFile(siteFile: SiteFile, directory: File): Unit = {
      val content: String = siteFile.content
      if (doWrite) Files.write(Files.file(directory, siteFile.url), content)
    }

    writeHtmlFile(new IndexObject(this), directory)
    writeHtmlFile(new TreeIndexObject(this), directory)
    writeHtmlFile(new NamesObject(this), directory)

    Files.deleteFiles(new File(directory, NotesObject.directoryName))
    for (note <- new NotesObject(this).noteFiles) {
      val content: String = note.content
      if (doWrite) Files.write(
        file = Files.file(directory, Seq(NotesObject.directoryName, s"${note.name}.html")),
        content
      )
    }

    Files.deleteFiles(new File(directory, EntityObject.directoryName))
    for (entity <- entities) writeHtmlFile(new EntityObject(this, entity), directory)

    Files.deleteFiles(new File(directory, Hierarchy.directoryName))
    val stores: Seq[WithPath[Store]] = withPath[Store](values = {
      case _: Collection | _: Document | _: Entities | _: EntityHolder | _: TeiHolder => Seq.empty
      case store => Seq(store)
    })
    for (store <- stores) writeHtmlFile(new HierarchyObject(this, store.path, store.value), directory)

    Files.deleteFiles(new File(directory, CollectionObject.directoryName))
    for (collection <- collections)
      writeHtmlFile(new CollectionObject(this, collection), directory)

    for {
      collection <- collections
      document <- collection.value.documents
      teiHolder: TeiHolder <- document.teiHolders
    } {
      val documentObject = new DocumentObject(this, collection, document, teiHolder)
      writeHtmlFile(documentObject, directory)
      writeSiteFile(documentObject.facsFile, directory)
    }

    Files.deleteFiles(new File(directory, ReportsObject.directoryName))
    writeHtmlFile(new ReportsObject(this), directory)
    writeHtmlFile(new MisnamedEntitiesReport(this), directory)
    writeHtmlFile(new NoRefsReport(this), directory)
  }

  def prettyPrintStore(): Unit = {
    for (entityHolder <- store.entities.get.by.get.stores)
      prettyPrint(entityHolder, Entity.toXmlElement(entityHolder.entity.copy(id = None)), Tei.prettyPrinter)
    prettyPrint(store)
  }

  private def prettyPrint(store: Store): Unit = {
    prettyPrint(store, Store.parsable.toXmlElement(store.asInstanceOf[Store.FromElement].element), Store.prettyPrinter)

    store match {
      case collection: Collection =>
        for (by <- collection.by; document <- by.stores; by <- document.by; teiHolder <- by.stores)
          prettyPrint(teiHolder, Tei.toXmlElement(teiHolder.tei), Tei.prettyPrinter)
      case _ =>
        for (by <- store.by; store <- by.stores) prettyPrint(store)
    }
  }

  private def prettyPrint(store: Store, toXml: => Elem, prettyPrinter: PrettyPrinter): Unit =
    for (fromUrl <- store.urls.fromUrl) if (Files.isFile(fromUrl)) Files.write(
      file = Files.url2file(fromUrl),
      content = prettyPrinter.renderXml(toXml)
    )
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

  private def mkSiteParameters: SiteParameters = new SiteParameters(
    title = "Документы",
    author = "www.alter-rebbe.org",
    email = "dub@opentorah.org",
    faviconJpg = "alter-rebbe",
    googleAnalyticsId = Some("UA-154490117-1"),
    navigationLinks = Seq(
      NavigationLink("/names", "Имена", Some(Viewer.Names)),
      NavigationLink("/collections", "Архивы", Some(Viewer.Collection)),
      NavigationLink("/notes/help", "Помощь", Some(Viewer.Collection)),
      NavigationLink("/notes/about", "О сайте", Some(Viewer.Collection))
    ),
    footerCol3 =
      // TODO when I use <p> instead of <span>, it gets styled by the TEI CSS - althought it is in the XHTML namespace?!
      <span>
        documents related to early Chabad history<br/>
        🄯 <a href="http://www.opentorah.org/" target={Viewer.Collection.name}>the Open Torah Project</a>
        <a href="http://creativecommons.org/licenses/by/4.0/" target={Viewer.Collection.name}>CC BY 4.0</a>
      </span>,
    homeTarget = Viewer.Collection,
    githubUsername = None, // Some("opentorah"),
    twitterUsername = None
  )

  val addPublicationStatement: Tei.Transformer = Tei.addPublicationStatement(
    publisher = new Publisher.Value(<ptr xmlns={Tei.namespace.uri} target="www.alter-rebbe.org"/>),
    status = "free",
    licenseName = "Creative Commons Attribution 4.0 International License",
    licenseUrl = "http://creativecommons.org/licenses/by/4.0/"
  )

  val addSourceDesc: Tei.Transformer = Tei.addSourceDesc(new SourceDesc.Value(<p>Facsimile</p>))
}
