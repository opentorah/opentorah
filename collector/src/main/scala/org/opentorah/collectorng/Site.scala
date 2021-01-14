package org.opentorah.collectorng

import org.opentorah.metadata.{Language, Names}
import org.opentorah.tei.{EntityReference, Page, SourceDesc, Tei, TeiRawXml, Title}
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Element, FromUrl, LinkResolver, Parsable, Parser, PrettyPrinter, Unparser, Xhtml,
  Xml}
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

  private val collections: Seq[Collection] = Site.getCollections(by)

  private val alias2collectionAlias: Map[String, CollectionAlias] =
    collections
    .filter(_.alias.isDefined)
    .map(collection => collection.alias.get -> new CollectionAlias(collection))
    .toMap

  override def findByName(name: String): Option[Store] =
    alias2collectionAlias.get(name).orElse(Store.findByName(name, stores))

  private val stores: Seq[Store] = Seq(byEntity, byEntityList, byNote, by)

  private val references: ListFile[EntityReference, References] = new ListFile[EntityReference, References](
    url = Files.fileInDirectory(fromUrl.url, "references-generated.xml"),
    name = "references",
    entry = EntityReference,
    wrapper = new References(_)
  )

  private lazy val navigationLinks: Seq[Html.NavigationLink] = pages.map { url =>
    val path: Store.Path = resolve(url).get
    val htmlContent: HtmlContent = path.last.asInstanceOf[HtmlContent]
    new Html.NavigationLink(
      Files.mkUrl(staticUrl(path)), // TODO when non-static - url
      htmlContent.htmlTitle.getOrElse("NO TITLE"),
      htmlContent.viewer
    )
  }

  // TODO:
  //  - reports
  //  - hierarchy root
  //  - hierarchy - at "by"

  // TODO return content type also.
  def content(path: Store.Path): String = path.last match {
    case teiFacet: Document.TeiFacet =>
      Site.prettyPrinter.render(teiFacet.content(this))

    case htmlContent: HtmlContent =>
      // TODO add - and use - resolver parameter!
      Site.prettyPrinter.render(doctype = Xhtml, element = Html.toHtml(
        lang = htmlContent.lang.getOrElse("ru"),
        viewer = htmlContent.viewer,
        title = htmlContent.htmlTitle,
        style = if (htmlContent.isWide) "wide" else "main",
        favicon,
        googleAnalyticsId,
        content = htmlContent.content(this),
        header = Html.header(
          this.title.xml,
          this.navigationLinks ++ htmlContent.navigationLinks
        ),
        footer = Html.footer(
          author = siteUrl,
          email,
          githubUsername,
          twitterUsername,
          footer.xml
        )
      ))
  }

  def headerSummary(collection: Collection): Seq[Xml.Node] = ??? // TODO

  // TODO move into Collection
  def collectionUrl(collection: Collection): Seq[String] = ???

  // TODO move into Collection
  def pageUrl(collection: Collection, document: Document, page: Page): Seq[String] = ???

  def writeLists(): Unit = {
    byEntity.writeDirectory()
    byNote.writeDirectory()
    references.write(References.fromSite(this))

    for (collection <- collections) collection.writeDirectory()
  }

  def verify(): Unit = {
    val errors: Seq[String] = references.get.verify(this)
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))
  }

  private val staticCollectionsRoot: String = "collections"

  def staticUrl(path: Seq[Store]): Seq[String] = path.last match {
    case note: Note => Seq(byNote.directory, note.name + ".html")
    case _: ByEntityList => Seq(byEntity.directory + ".html")
    case _: ByHierarchy if path.length == 1 => Seq("collections.html")

    case htmlFacet: Document.HtmlFacet =>
      Seq(staticCollectionsRoot, htmlFacet.collection.alias.get, "documents", htmlFacet.withExtension)

    case facsFacet: Document.FacsFacet =>
      Seq(staticCollectionsRoot, facsFacet.collection.alias.get, "facs", facsFacet.withExtension)

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
      } { path: Store.Path =>
        val viewer: Html.Viewer = path.last match {
          case htmlContent: HtmlContent => htmlContent.viewer
          case _ => Html.Viewer.default
        }
        Some(LinkResolver.Resolved(
          url = staticUrl(path),
          role = Some(viewer.name)
        ))
      }

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

  private def getCollections(by: ByHierarchy): Seq[Collection] = by.stores.flatMap {
    case collection: Collection => Seq(collection)
    case hierarchy: Hierarchy => hierarchy.by.toSeq.flatMap(getCollections)
    case _ => Seq.empty
  }

  def read(rootPath: String): Site =
    read(new File(rootPath).toURI.toURL)

  def read(rootUrl: URL): Site = {
    val directory: URL = Files.subdirectory(rootUrl, "store")
    Parser.parseDo(Site.parse(Files.fileInDirectory(directory, "storeng.xml")))
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

  private val siteUrlAttribute: Attribute.Required[String] = Attribute("siteUrl").required
  private val facsimilesUrlAttribute: Attribute.Required[String] = Attribute("facsimilesUrl").required
  private val faviconAttribute: Attribute.Required[String] = Attribute("favicon").required
  private val licenseNameAttribute: Attribute.Required[String] = Attribute("licenseName").required
  private val licenseUrlAttribute: Attribute.Required[String] = Attribute("licenseUrl").required
  private val googleAnalyticsIdAttribute: Attribute.Optional[String] = Attribute("googleAnalyticsId").optional
  private val emailAttribute: Attribute.Required[String] = Attribute("email").required
  private val githubUsernameAttribute: Attribute.Optional[String] = Attribute("githubUsername").optional
  private val twitterUsernameAttribute: Attribute.Optional[String] = Attribute("twitterUsername").optional

  object Page extends Element[String]("page") {
    private val urlAttribute: Attribute.Required[String] = Attribute("url").required
    override def contentParsable: Parsable[String] = urlAttribute
  }

  object Footer extends TeiRawXml("footer")

  override def contentParsable: Parsable[Site] = new Parsable[Site] {
    override def parser: Parser[Site] = for {
      fromUrl <- Element.currentFromUrl
      names <- Names.withDefaultNameParsable()
      title <- Title.element.required()
      siteUrl <- siteUrlAttribute()
      facsimilesUrl <- facsimilesUrlAttribute()
      favicon <- faviconAttribute()
      sourceDesc <- SourceDesc.element.required()
      navigationLinks <- Page.seq()
      licenseName <- licenseNameAttribute()
      licenseUrl <- licenseUrlAttribute()
      googleAnalyticsId <- googleAnalyticsIdAttribute()
      email <- emailAttribute()
      githubUsername <- githubUsernameAttribute()
      twitterUsername <- twitterUsernameAttribute()
      footer <- Footer.element.required()
      byEntity <- ByEntity.required()
      byEntityList <- ByEntityList.required()
      byNote <- ByNote.required()
      by <- ByHierarchy.followRedirects.required()
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

    override def unparser: Unparser[Site] = Unparser.concat[Site](
      Names.withDefaultNameParsable(_.names),
      Title.element.required(_.title),
      siteUrlAttribute(_.siteUrl),
      facsimilesUrlAttribute(_.facsimilesUrl),
      faviconAttribute(_.favicon),
      SourceDesc.element.required(_.sourceDesc),
      Page.seq(_.pages),
      licenseNameAttribute(_.licenseName),
      licenseUrlAttribute(_.licenseUrl),
      googleAnalyticsIdAttribute(_.googleAnalyticsId),
      emailAttribute(_.email),
      githubUsernameAttribute(_.githubUsername),
      twitterUsernameAttribute(_.twitterUsername),
      Footer.element.required(_.footer),
      ByEntity.required(_.byEntity),
      ByEntityList.required(_.byEntityList),
      ByNote.required(_.byNote),
      ByHierarchy.required(_.by)
    )
  }
}
