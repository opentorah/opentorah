package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{Availability, CalendarDesc, EntityReference, LangUsage, Language, ProfileDesc, PublicationStmt,
  Publisher, SourceDesc, Tei, TeiRawXml, Title, Entity => TeiEntity}
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Element, FromUrl, Html, LinkResolver, Parsable, Parser, PrettyPrinter, Unparser, Xml}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.net.URL

// TODO use enumerated Attributes!
// TODO figure out how to read/write with materialized redirects,
//   pre-generate hierarchy in one file and pretty-print hierarchy stores!
final class Site(
  override val fromUrl: FromUrl,
  override val names: Names,
  val title: Title.Value,
  val siteUrl: String,
  val facsimilesUrl: String,
  val favicon: String,
  val sourceDesc: SourceDesc.Value,
  val calendarDesc: CalendarDesc.Value,
  val pages: Seq[String],
  val licenseName: String,
  val licenseUrl: String,
  val googleAnalyticsId: Option[String],
  val email: String,
  val githubUsername: Option[String],
  val twitterUsername: Option[String],
  val footer: Site.Footer.Value,
  val entities: Entities,
  val entityLists: EntityLists,
  val notes: Notes,
  val by: ByHierarchy
) extends Store with FromUrl.With {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private val paths: Seq[Store.Path] = getPaths(Seq.empty, by)

  private def getPaths(path: Store.Path, by: ByHierarchy): Seq[Store.Path] = by.stores.flatMap { store =>
    val storePath: Store.Path = path ++ Seq(by, store)
    Seq(storePath) ++ (store match {
      case hierarchy : Hierarchy  => getPaths(storePath, hierarchy.by)
      case _ => Seq.empty
    })
  }

  val store2path: Map[Store, Store.Path] = (for (path <- paths) yield path.last -> path).toMap

  val collections: Seq[Collection] = for {
    path <- paths
    last = path.last
    if last.isInstanceOf[Collection]
  } yield last.asInstanceOf[Collection]

  val hierarchies: Seq[Hierarchy] = for {
    path <- paths
    last = path.last
    if last.isInstanceOf[Hierarchy]
  } yield last.asInstanceOf[Hierarchy]

  val alias2collectionAlias: Map[String, Collection.Alias] = collections
    .filter(_.alias.isDefined)
    .map(collection => collection.alias.get -> new Collection.Alias(collection))
    .toMap

  private val references: ListFile[EntityReference, References] = new ListFile[EntityReference, References](
    url = Files.fileInDirectory(fromUrl.url, "references-generated.xml"),
    name = "references",
    entry = EntityReference,
    wrapper = new References(_)
  )

  def getReferences: References = references.get

  def resolve(url: String): Option[Store.Path] = resolve(Files.splitAndDecodeUrl(url))

  def resolve(url: Seq[String]): Option[Store.Path] =
    Store.resolve(fromStaticPath(url), this, Seq.empty)

  override def findByName(name: String): Option[Store] =
    alias2collectionAlias.get(name).orElse(Store.findByName(name, stores))

  private val stores: Seq[Store] = Seq(Index.Flat, Index.Tree, entities, entityLists, notes, Reports, by)

  private lazy val navigationLinks: Seq[Xml.Element] = pages.map { url =>
    val path: Store.Path = resolve(url).get
    a(path)(text = path.last.asInstanceOf[HtmlContent].htmlHeadTitle.getOrElse("NO TITLE"))
  }

  def a(path: Store.Path, part: Option[String] = None): Html.a = Html.a(
    path = toStaticPath(path),
    part = part,
    target = Some((path.last match {
      case htmlContent: HtmlContent => htmlContent.viewer
      case _ => Viewer.default
    }).name)
  )

  private def toStaticPath(path: Seq[Store]): Seq[String] = path.last match {
    case collectionAlias: Collection.Alias => Seq("collections", collectionAlias.alias, "index.html")
    case htmlFacet: Document.TextFacet => Seq("collections", htmlFacet.collection.alias.get, "documents", htmlFacet.withExtension)
    case facsFacet: Document.FacsimileFacet => Seq("collections", facsFacet.collection.alias.get, "facs", facsFacet.withExtension)
    case _: EntityLists => Seq("names.html")
    case entity: Entity => Seq("names", entity.name + ".html")
    case _: Notes => Seq("notes", "index.html")
    case note: Note => Seq("notes", note.name + ".html")
    case Reports => Seq("reports", "index.html")
    case report: Report[_] => Seq("reports", report.name + ".html")
    case _ if path.head == by => "by" +: path.map(_.structureName) :+ "index.html"
    case _ => path.map(_.structureName)
  }

  private def fromStaticPath(path: Seq[String]): Seq[String] = {
    if (path.isEmpty) Seq("index.html")
    else if (path.head == "by") path.tail
    else path match {
      case Seq("collections", alias, "index.html") => Seq(alias)
      case Seq("collections", alias, "documents", document) => Seq(alias, "document" , document)
      case Seq("collections", alias, "facs"     , document) => Seq(alias, "facsimile", document)
      case Seq("names.html") => Seq(entityLists.names.name)
      case Seq("names"  , entity) => Seq(entities.names.name, entity)
      case Seq("notes") => Seq(notes.names.name)
      case Seq("notes"  , "index.html") => Seq(notes.names.name)
      case Seq("notes"  , note) => Seq(notes.names.name, note)
      case Seq("reports") => Seq(Reports.names.name)
      case Seq("reports", "index.html") => Seq(Reports.names.name)
      case Seq("reports", report) => Seq(Reports.names.name, report)
      case _ => path
    }
  }

  def content(path: Store.Path): (String, Boolean) = path.lastOption.getOrElse(this) match {
    case teiFacet: Document.TeiFacet => (getTeiContent(teiFacet.getTei), true)
    case htmlContent: HtmlContent => (getHtmlContent(htmlContent), false)
  }

  private def getTeiContent(tei: Tei): String = {
    val result: Tei = tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = Some(new PublicationStmt(
          publisher = Some(new Publisher.Value(<ptr target={siteUrl}/>)),
          availability = Some(new Availability(
            status = Some("free"),
            xml = <licence><ab><ref n="license" target={licenseUrl}>{licenseName}</ref></ab></licence>
          ))
        )),
        sourceDesc = Some(sourceDesc)
      ),
      profileDesc = Some(tei.teiHeader.profileDesc.getOrElse(ProfileDesc.empty).copy(
        langUsage = Some(LangUsage(languages = Seq(Language(
          ident = tei.text.lang.get,
          usage = None,
          text = None
        )))),
        calendarDesc = Some(calendarDesc)
      ))
    ))

    Tei.renderXml(result)
  }

  private def getHtmlContent(htmlContent: HtmlContent): String = {
    val content: Xml.Element = Tei.toHtml(
      linkResolver(htmlContent match {
        case htmlFacet: Document.TextFacet => Some(htmlFacet)
        case _ => None
      }),
      htmlContent.content(this)
    )
    val navigationLinks: Seq[Xml.Element] = htmlContent.navigationLinks(this)

    val html: Xml.Element = HtmlTheme.toHtml(
      lang = htmlContent.lang.getOrElse("ru"),
      viewer = htmlContent.viewer,
      headTitle = htmlContent.htmlHeadTitle,
      title = htmlContent.htmlBodyTitle,
      style = if (htmlContent.isWide) "wide" else "main",
      favicon,
      googleAnalyticsId,
      content,
      header = HtmlTheme.header(
        this.title.xml,
        this.navigationLinks ++ navigationLinks
      ),
      footer = HtmlTheme.footer(
        author = siteUrl,
        email,
        githubUsername,
        twitterUsername,
        footer.xml
      )
    )

    Site.htmlPrettyPrinter.render(doctype = Html, element = html)
  }

  private def linkResolver(textFacet: Option[Document.TextFacet]): LinkResolver = new LinkResolver {
    private val facsUrl: Option[Store.Path] = textFacet.map(textFacet =>
      textFacet.collection.facsimileFacet.of(textFacet.document).path(Site.this))

    def toResolved(a: Option[Html.a], error: => String): Option[Html.a] = {
      if (a.isEmpty) logger.warn(error)
      a
    }

    override def resolve(url: Seq[String]): Option[Html.a] = toResolved(
      Site.this.resolve(url).map(path => a(path)),
      s"did not resolve: $url"
    )

    override def findByRef(ref: String): Option[Html.a] = toResolved(
      entities.findByName(ref).map(entity => entity.a(Site.this)),
      s"did not find reference: $ref"
    )

    override def facs(pageId: String): Option[Html.a] = toResolved(
      facsUrl.map(facsUrl => a(facsUrl, part = Some(pageId))),
      "did not get facsimile: $pageId"
    )
  }

  def writeLists(): Unit = {
    entities.writeDirectory()
    notes.writeDirectory()
    for (collection <- collections) collection.writeDirectory()

    // Collection lists must exist by the time this runs:
    references.write(References.fromSite(this))
  }

  def prettyPrint(): Unit = {
    for (entity <- entities.directoryEntries) entities.writeFile(
      entity,
      content = Store.renderXml(TeiEntity.xmlElement(entities.getFile(entity)))
    )

    for (collection <- collections) Files.write(
      file = Files.url2file(collection.fromUrl.url),
      content = Store.renderXml(Collection, collection)
    )

    for {
      collection <- collections
      document <- collection.directoryEntries
    } collection.writeFile(
      document,
      content = Tei.renderXml(collection.getFile(document))
    )
  }

  def verify(): Unit = {
    val errors: Seq[String] = getReferences.verify(this)
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))
  }

  def writeStaticFiles(): Unit = {
    val directory: File = Files.url2file(Files.getParent(fromUrl.url))

    def deleteDirectory(name: String): Unit = Files.deleteFiles(new File(directory, name))

    def write(htmlContent: HtmlContent): Unit = Files.write(
      Files.file(directory, toStaticPath(htmlContent.path(this))),
      getHtmlContent(htmlContent)
    )

    // Index
    write(Index.Flat)
    write(Index.Tree)
    write(entityLists)

    // Hierarchy
    deleteDirectory("by")
    for (hierarchy <- hierarchies) write(hierarchy)

    // Names
    deleteDirectory("names")
    for (entity <- entities.directoryEntries) write(entity)

    // Notes
    deleteDirectory("notes")
    write(notes)
    for (note <- notes.directoryEntries) write(note)

    // Reports
    deleteDirectory("reports")
    write(Reports)
    for (report <- Reports.reports) write(report)

    // Documents, facsimile viewers and collection indices
    deleteDirectory("collections")

    // Collection indices
    for (collection <- collections) write(collection)

    // Documents
    for {
      collection <- collections
      document <- collection.directoryEntries
    } {
      write(collection.textFacet     .of(document))
      write(collection.facsimileFacet.of(document))
    }
  }
}

object Site extends Element[Site]("site") {

  def read(rootPath: String): Site =
    read(new File(rootPath).toURI.toURL)

  def read(rootUrl: URL): Site = {
    val directory: URL = Files.subdirectory(rootUrl, "store")
    Parser.parseDo(Site.parse(Files.fileInDirectory(directory, "site.xml")))
  }

  val htmlPrettyPrinter: PrettyPrinter = Tei.prettyPrinter.copy(
    alwaysStackElements = Tei.prettyPrinter.alwaysStackElements ++ Set("nav", "header", "main", "div", "store"),
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
      calendarDesc <- CalendarDesc.element.required()
      navigationLinks <- Page.seq()
      licenseName <- licenseNameAttribute()
      licenseUrl <- licenseUrlAttribute()
      googleAnalyticsId <- googleAnalyticsIdAttribute()
      email <- emailAttribute()
      githubUsername <- githubUsernameAttribute()
      twitterUsername <- twitterUsernameAttribute()
      footer <- Footer.element.required()
      entities <- Entities.required()
      entityLists <- EntityLists.required()
      notes <- Notes.required()
      by <- ByHierarchy.followRedirects.required()
    } yield new Site(
      fromUrl,
      names,
      title,
      siteUrl,
      facsimilesUrl,
      favicon,
      sourceDesc,
      calendarDesc,
      navigationLinks,
      licenseName,
      licenseUrl,
      googleAnalyticsId,
      email,
      githubUsername,
      twitterUsername,
      footer,
      entities,
      entityLists,
      notes,
      by
    )

    override def unparser: Unparser[Site] = Unparser.concat[Site](
      Names.withDefaultNameParsable(_.names),
      Title.element.required(_.title),
      siteUrlAttribute(_.siteUrl),
      facsimilesUrlAttribute(_.facsimilesUrl),
      faviconAttribute(_.favicon),
      SourceDesc.element.required(_.sourceDesc),
      CalendarDesc.element.required(_.calendarDesc),
      Page.seq(_.pages),
      licenseNameAttribute(_.licenseName),
      licenseUrlAttribute(_.licenseUrl),
      googleAnalyticsIdAttribute(_.googleAnalyticsId),
      emailAttribute(_.email),
      githubUsernameAttribute(_.githubUsername),
      twitterUsernameAttribute(_.twitterUsername),
      Footer.element.required(_.footer),
      Entities.required(_.entities),
      EntityLists.required(_.entityLists),
      Notes.required(_.notes),
      ByHierarchy.required(_.by)
    )
  }
}
