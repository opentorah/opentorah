package org.opentorah.collectorng

import org.opentorah.collectorng.Site.viewer
import org.opentorah.metadata.{Language, Names}
import org.opentorah.tei.{Page, SourceDesc, Tei, TeiRawXml, Title}
import org.opentorah.util.Files
import org.opentorah.xml.{Antiparser, Attribute, Element, FromUrl, LinkResolver, Parser, PrettyPrinter, Xhtml, Xml}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.net.URL

// TODO use enumerated Attributes!

final class Site(
  override val fromUrl: FromUrl,
  override val names: Names,
  val title: Title.Value,
  val siteUrl: String,
  val facsimilesUrl: String,
  val favicon: String,
  val sourceDesc: SourceDesc.Value,
  val pages: Seq[String],
  val licenseName: String,
  val licenseUrl: String,
  val googleAnalyticsId: Option[String],
  val email: String,
  val githubUsername: Option[String],
  val twitterUsername: Option[String],
  val footer: Site.Footer.Value,
  val byEntity: ByEntity,
  val byEntityList: ByEntityList,
  val byNote: ByNote,
  val by: ByHierarchy
) extends Store with FromUrl.With {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private val collection2path: Map[Collection, Store.Path] = by
    .collections(Seq.empty)
    .map(path => path.last.asInstanceOf[Collection] -> path.init)
    .toMap

  private val collections: Seq[Collection] = collection2path.keys.toSeq

  private val collection2collectionAlias: Map[Collection, CollectionAlias] = collections
    .filter(_.alias.isDefined)
    .map(collection => collection -> new CollectionAlias(collection))
    .toMap

  private val alias2collectionAlias: Map[String, CollectionAlias] = collection2collectionAlias.values
    .map(collectionAlias => collectionAlias.alias -> collectionAlias)
    .toMap

  override def findByName(name: String): Option[Store] =
    alias2collectionAlias.get(name).orElse(Store.findByName(name, stores))

  private val stores: Seq[Store] = Seq(byEntity, byEntityList, byNote, by)

  private lazy val navigationLinks: Seq[Html.NavigationLink] = pages.map { url =>
    val path: Store.Path = resolve(url).get
    new Html.NavigationLink(
      Files.mkUrl(staticUrl(path)), // TODO when non-static - url
      Site.title(path.last).getOrElse("NO TITLE"),
      Site.viewer(path.last)
    )
  }

  // TODO:
  //  - reports
  //  - hierarchy root
  //  - hierarchy - at "by"

  def content(path: Store.Path): String = path.last match {
    // TODO except for the TeiFacet, everything is htmlContent;
    // return content type also.
    case note: Note => htmlContent(path.last, byNote.getFile(note.name).html)
    case collectionAlias: CollectionAlias => collectionIndexContent(collectionAlias.collection)
    case collection     : Collection      => collectionIndexContent(collection)

    case documentFacet: Document.Facet => documentFacet.facet match {
      case facet: Collection.TeiFacet =>
        Site.prettyPrinter.render(Tei.toXmlElement(facet.collection.getFile(documentFacet.document.name)))
      case facet: Collection.FacsFacet =>
        Site.prettyPrinter.render(facsContent(facet.collection, documentFacet.document))
      case facet: Collection.HtmlFacet => ???
        Site.prettyPrinter.render(htmlContent(facet.collection, documentFacet.document))
    }

    case hierarchy: Hierarchy => ???
    case entity: Entity => ???
    case by: By => ???
    case store: Store => ???
  }

  private def facsContent(collection: Collection, document: Document): Xml.Element = {
    <div class={Html.Viewer.Facsimile.name}>
      {headerSummary(collection)}
      <div class="facsimileScroller">{
        for (page: Page <- document.pages(collection.pageType).filterNot(_.pb.isMissing)) yield {
          val n: String = page.pb.n
          <a target={Html.Viewer.Document.name} href={Files.mkUrl(pageUrl(collection, document, page))}>
            <figure>
              <img
                id={Page.pageId(n)}
                alt={s"facsimile for page $n"}
                src={page.pb.facs.getOrElse(facsimilesUrl + collectionUrl(collection) + "/" + n + ".jpg")}
              />
              <figcaption>{n}</figcaption>
            </figure>
          </a>
        }}</div>
    </div>
  }

  private def htmlContent(collection: Collection, document: Document): Xml.Element = ???

  private def headerSummary(collection: Collection): Seq[Xml.Node] = ??? // TODO

  private def collectionIndexContent(collection: Collection): String = {
    val path: Store.Path = collection2path(collection)
    collection.indexContent(path)
  }

  private def htmlContent(store: Store, content: Xml.Element): String = {
    val html: Xml.Element = Html.toHtml(
      Site.lang  (store),
      Site.viewer(store),
      Site.title (store),
      Site.style (store),
      favicon,
      googleAnalyticsId,
      content,
      Html.header(
        this.title.xml,
        this.navigationLinks ++ Site.navigationLinks(store)
      ),
      Html.footer(
        author = siteUrl,
        email,
        githubUsername,
        twitterUsername,
        footer.xml
      )
    )
    // TODO in the old generation, links were (?) resolved and targets added - probably more than once ;)
    Site.prettyPrinter.render(doctype = Xhtml, element = html)
  }

  private def collectionUrl(collection: Collection): Seq[String] = ???
  private def pageUrl(collection: Collection, document: Document, page: Page): Seq[String] = ???

  def writeLists(): Unit = {
    // Write lists
    if (Files.isFileUrl(fromUrl.url)) {
      byEntity.writeDirectory()
      byNote.writeDirectory()

      for (collection <- collections) collection.writeDirectory()
    }
  }

  private val staticCollectionsRoot: String = "collections"

  def staticUrl(path: Seq[Store]): Seq[String] = path.last match {
    case note: Note => Seq(byNote.directory, note.name + ".html")
    case _: ByEntityList => Seq(byEntity.directory + ".html")
    case _: ByHierarchy if path.length == 1 => Seq("collections.html")

    case documentFacet: Document.Facet =>
      val facetDirectory: String = documentFacet.facet match {
        case _: Collection.HtmlFacet => "documents"
        case _: Collection.FacsFacet => "facs"
      }
      Seq(staticCollectionsRoot, documentFacet.facet.collection.alias.get, facetDirectory, documentFacet.withExtension)

    case _ => path.map (_.names.doFind (Language.English.toSpec).name)
  }

  def writeStaticFiles(): Unit = {
    val directory: File = Files.url2file(Files.getParent(fromUrl.url))

    def writeStore(store: Store): Unit = writePath(Seq(store))

    def writePath(path: Store.Path): Unit = Files.write(
      Files.file(directory, staticUrl(path)),
      content(path)
    )

    // Notes
    Files.deleteFiles(new File(directory, byNote.directory))
    for (note <- byNote.directoryEntries) writeStore(note)

    // Documents, facsimile viewers and collection indices
//    // TODO uncomment once all Collection-related files are written here
//    //Files.deleteFiles(new File(directory, staticCollectionsRoot))
//    //for (collection <- collections)
//    // TODO WRITE COLLECTION INDEX
    for {
      collection <- collections
      document <- collection.documents
    } {
//      // TODO WRITE HTML FACET collection.htmlFacet.of(document)
//      writeStore(collection.facsimileFacet.of(document))
    }
  }

  def resolve(url: String): Option[Store.Path] = resolve(Files.splitUrl(url))

  def resolve(url: Seq[String]): Option[Store.Path] = Store.resolve(url, this, Seq.empty)

  // TODO add default target to the non-local links...

  def resolver(facsUrl: Seq[String]): LinkResolver = new LinkResolver {

    override def resolve(url: Seq[String]): Option[LinkResolver.Resolved] =
      Site.this.resolve(url).fold[Option[LinkResolver.Resolved]] {
        logger.warn(s"did not resolve: $url")
        None
      } { path: Store.Path => Some(LinkResolver.Resolved(
        url = staticUrl(path),
        role = Some(viewer(path.last).name)
      ))}

    override def findByRef(ref: String): Option[LinkResolver.Resolved] =
      byEntity.findByName(ref).map { entity: Entity => Some(LinkResolver.Resolved(
        url = staticUrl(Seq(byEntity, entity)),
        role = Some(Html.Viewer.Names.name)
      ))}.getOrElse {
        logger.warn(s"did not find reference: $ref")
        None
      }

    override def facs: LinkResolver.Resolved = LinkResolver.Resolved(
      url = facsUrl,
      role = Some(Html.Viewer.Facsimile.name)
    )
  }
}

object Site extends Element[Site]("site") {

  def read(rootPath: String): Site =
    read(new File(rootPath).toURI.toURL)

  def read(rootUrl: URL): Site = {
    val directory: URL = Files.subdirectory(rootUrl, "store")
    Parser.parseDo(Site.parse(Files.fileInDirectory(directory, "storeng.xml")))
  }

  // TODO Option[Html.Viewer]!
  private def viewer(store: Store): Html.Viewer = store match {
    case _: Collection           => Html.Viewer.Collection
    case _: Collection.HtmlFacet => Html.Viewer.Document
    case _: Collection.FacsFacet => Html.Viewer.Facsimile
    case _: ByEntityList         => Html.Viewer.Names
    case _: ByEntity             => Html.Viewer.Names
    case _: Entity               => Html.Viewer.Names
    case _                       => Html.Viewer.default
  }

  private def style(store: Store): String = store match {
    case _: Collection => "wide"
    case _             => "main"
  }

  private def lang(store: Store): String = store match {
    case document: Document.Facet if document.facet.isInstanceOf[Collection.HtmlFacet] => document.document.lang
    case _ => "ru"
  }

  private def navigationLinks(store: Store): Seq[Html.NavigationLink] = store match {
    case _ => Seq.empty
  }

  private def title(store: Store): Option[String] = store match {
    case hierarchy: Hierarchy => hierarchy.title.map(_.xml2string)
    case collection: Collection => collection.title.map(_.xml2string)
    // TODO document
    case note: Note => note.title
    case by: By => by.selector.title
    //    case store => Some(store.names.doFind(Language.Russian.toSpec).name)
    case _ => None
  }

  private val prettyPrinter: PrettyPrinter = Tei.prettyPrinter.copy(
    alwaysStackElements = Tei.prettyPrinter.alwaysStackElements ++ Set("nav", "header", "main", "div"),
    // Note: empty elements are mis-processed by the browser (next element gets inserted inside the empty one!),
    // so I make sure there are no empty elements in the HTML:
    allowEmptyElements = false,
    // ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    // and in general, it's weird to expand the elements that are always empty:
    keepEmptyElements = Set("br", "meta", "link", "img", "data")
  )

  private val siteUrlAttribute: Attribute[String] = Attribute("siteUrl")
  private val facsimilesUrlAttribute: Attribute[String] = Attribute("facsimilesUrl")
  private val faviconAttribute: Attribute[String] = Attribute("favicon")
  private val licenseNameAttribute: Attribute[String] = Attribute("licenseName")
  private val licenseUrlAttribute: Attribute[String] = Attribute("licenseUrl")
  private val googleAnalyticsIdAttribute: Attribute[String] = Attribute("googleAnalyticsId")
  private val emailAttribute: Attribute[String] = Attribute("email")
  private val githubUsernameAttribute: Attribute[String] = Attribute("githubUsername")
  private val twitterUsernameAttribute: Attribute[String] = Attribute("twitterUsername")

  object Page extends Element[String]("page") {
    private val urlAttribute: Attribute[String] = Attribute("url")
    override def parser: Parser[String] = urlAttribute.required
    override def antiparser: Antiparser[String] = urlAttribute.toXml
  }

  object Footer extends TeiRawXml("footer")

  override def parser: Parser[Site] = for {
    fromUrl <- currentFromUrl
    names <- Names.withDefaultNameParser
    title <- Title.parsable.required
    siteUrl <- siteUrlAttribute.required
    facsimilesUrl <- facsimilesUrlAttribute.required
    favicon <- faviconAttribute.required
    sourceDesc <- SourceDesc.parsable.required
    navigationLinks <- Page.all
    licenseName <- licenseNameAttribute.required
    licenseUrl <- licenseUrlAttribute.required
    googleAnalyticsId <- googleAnalyticsIdAttribute.optional
    email <- emailAttribute.required
    githubUsername <- githubUsernameAttribute.optional
    twitterUsername <- twitterUsernameAttribute.optional
    footer <- Footer.parsable.required
    byEntity <- ByEntity.required
    byEntityList <- ByEntityList.required
    byNote <- ByNote.required
    by <- ByHierarchy.followRedirects.required
  } yield new Site(
    fromUrl,
    names,
    title,
    siteUrl,
    facsimilesUrl,
    favicon,
    sourceDesc,
    navigationLinks,
    licenseName,
    licenseUrl,
    googleAnalyticsId,
    email,
    githubUsername,
    twitterUsername,
    footer,
    byEntity,
    byEntityList,
    byNote,
    by
  )

  override def antiparser: Antiparser[Site] = Antiparser.concat[Site](
    Names.antiparser(_.names),
    Title.parsable.toXml(_.title),
    siteUrlAttribute.toXml(_.siteUrl),
    facsimilesUrlAttribute.toXml(_.facsimilesUrl),
    faviconAttribute.toXml(_.favicon),
    SourceDesc.parsable.toXml(_.sourceDesc),
    Page.toXmlSeq(_.pages),
    licenseNameAttribute.toXml(_.licenseName),
    licenseUrlAttribute.toXml(_.licenseUrl),
    googleAnalyticsIdAttribute.toXmlOption(_.googleAnalyticsId),
    emailAttribute.toXml(_.email),
    githubUsernameAttribute.toXmlOption(_.githubUsername),
    twitterUsernameAttribute.toXmlOption(_.twitterUsername),
    Footer.parsable.toXml(_.footer),
    ByEntity.toXml(_.byEntity),
    ByEntityList.toXml(_.byEntityList),
    ByNote.toXml(_.byNote),
    ByHierarchy.toXml(_.by)
  )
}
