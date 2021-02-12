package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{Availability, CalendarDesc, EntityReference, EntityType, LangUsage, Language, ProfileDesc,
  PublicationStmt, Publisher, SourceDesc, Tei, TeiRawXml, Title, Unclear, Entity => TeiEntity}
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Element, FromUrl, Html, LinkResolver, Parsable, Parser, PrettyPrinter, Unparser, Xml}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.net.URL

// TODO figure out how to read/write with materialized redirects,
//   pre-generate hierarchy in one file and pretty-print hierarchy stores.
// TODO retrieve TEI(?) references from notes.
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

  private val references = WithSource(
    url = Files.fileInDirectory(fromUrl.url, "references-generated.xml"),
    name = "references",
    value = EntityReference
  )
  def getReferences: Seq[WithSource[EntityReference]] = references.get

  private val unclears = WithSource(
    url = Files.fileInDirectory(fromUrl.url, "unclears-generated.xml"),
    name = "unclears",
    value = Unclear.element
  )
  def getUnclears: Seq[WithSource[Unclear.Value]] = unclears.get

  def resolve(url: String): Option[Store.Path] = resolve(Files.splitAndDecodeUrl(url))

  def resolve(path: Seq[String]): Option[Store.Path] =
    if (path.isEmpty) Some(Seq(Index.Flat))
    else Store.resolve(path, this, Seq.empty)

  override def findByName(name: String): Option[Store] = alias2collectionAlias.get(name)
    .orElse(Store.findByName(name, Seq(entities, notes, Reports, by)))
    .orElse(Store.findByName(
      name,
      "html",
      name => Store.findByName(name, Seq(Index.Flat, Index.Tree, entityLists))
    ))

  private lazy val navigationLinks: Seq[Xml.Element] = pages.map { url =>
    val path: Store.Path = resolve(url).get
    a(path)(text = path.last.asInstanceOf[HtmlContent].htmlHeadTitle.getOrElse("NO TITLE"))
  }

  def a(path: Store.Path, part: Option[String] = None): Html.a = Html.a(
    path = path.map(_.structureName),
    part = part,
    target = Some((path.last match {
      case htmlContent: HtmlContent => htmlContent.viewer
      case _ => Viewer.default
    }).name)
  )

  def content(path: Store.Path): (String, Boolean) = path.lastOption.getOrElse(this) match {
    case teiFacet: Document.TeiFacet => (renderTeiContent(teiFacet.getTei), true)
    case htmlContent: HtmlContent => (renderHtmlContent(htmlContent), false)
  }

  private def renderTeiContent(tei: Tei): String = Tei.renderXml(
    tei.copy(teiHeader = tei.teiHeader.copy(
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
  )

  private def renderHtmlContent(htmlContent: HtmlContent): String =
    Site.htmlPrettyPrinter.render(doctype = Html, element = HtmlTheme.toHtml(
      viewer = htmlContent.viewer,
      headTitle = htmlContent.htmlHeadTitle,
      title = htmlContent.htmlBodyTitle,
      style = if (htmlContent.isWide) "wide" else "main",
      favicon,
      googleAnalyticsId,
      content = resolveHtmlContent(htmlContent),
      header = HtmlTheme.header(
        this.title.xml,
        this.navigationLinks ++ htmlContent.navigationLinks(this)
      ),
      footer = HtmlTheme.footer(
        author = siteUrl,
        email,
        githubUsername,
        twitterUsername,
        footer.xml
      )
    ))

  private def resolveHtmlContent(htmlContent: HtmlContent): Xml.Element = Tei.toHtml(
    linkResolver(htmlContent match {
      case htmlFacet: Document.TextFacet => Some(htmlFacet)
      case _ => None
    }),
    htmlContent.content(this)
  )

  private def linkResolver(textFacet: Option[Document.TextFacet]): LinkResolver = new LinkResolver {
    private val facsUrl: Option[Store.Path] = textFacet.map(textFacet =>
      textFacet.collection.facsimileFacet.of(textFacet.document).path(Site.this))

    def resolved(a: Option[Html.a], error: => String): Option[Html.a] = {
      if (a.isEmpty) Site.logger.warn(error)
      a
    }

    override def resolve(url: Seq[String]): Option[Html.a] = resolved(
      Site.this.resolve(url).map(path => a(path)),
      s"did not resolve: $url"
    )

    override def findByRef(ref: String): Option[Html.a] = resolved(
      entities.findByName(ref).map(entity => entity.a(Site.this)),
      s"did not find reference: $ref"
    )

    override def facs(pageId: String): Option[Html.a] = resolved(
      facsUrl.map(facsUrl => a(facsUrl, part = Some(pageId))),
      "did not get facsimile: $pageId"
    )
  }

  def build(withPrettyPrint: Boolean): Unit = {
    Site.logger.info("Writing site lists.")

    entities.writeDirectory()
    notes.writeDirectory()
    for (collection <- collections) collection.writeDirectory()

    // Collection lists must exist by the time this runs:

    Site.logger.info("Writing references.")
    references.write(WithSource.all[EntityReference](
      this,
      nodes => for {
        entityType <- EntityType.values
        node <- nodes
        descendants <- Xml.descendants(node, entityType.nameElement, EntityReference)
      } yield descendants
    ))

    Site.logger.info("Writing unclears.")
    unclears.write(WithSource.all[Unclear.Value](
      this,
      nodes => (for (node <- nodes) yield Xml.descendants(node, Unclear.element.elementName, Unclear.element)).flatten
    ))

    Site.logger.info("Verifying site.")

    val errors: Seq[String] = getReferences.flatMap { value =>
      val reference: EntityReference = value.value
      val name: Xml.Nodes = reference.name
      reference.ref.fold[Option[String]](None) { ref =>
        if (ref.contains(" ")) Some(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
          entities.findByName(ref).fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
            if (named.entityType == reference.entityType) None
            else Some(s"${reference.entityType} reference to ${named.entityType} ${named.name}: $name [$ref]")
          }
        }
      }
    }

    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))

    // detect and log unresolved references
    for (htmlContent <- hierarchies ++ collections ++ notes.directoryEntries ++ entities.directoryEntries)
      resolveHtmlContent(htmlContent)

    for {
      collection <- collections
      document <- collection.directoryEntries
      text = collection.textFacet.of(document)
    } resolveHtmlContent(text)

    Site.logger.info("Pretty-printing site.")
    if (withPrettyPrint) prettyPrint()
  }

  private def prettyPrint(): Unit = {
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
}

object Site extends Element[Site]("site") {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val htmlPrettyPrinter: PrettyPrinter = Tei.prettyPrinter.copy(
    alwaysStackElements = Tei.prettyPrinter.alwaysStackElements ++ Set("nav", "header", "main", "div", "store"),
    // Note: only the ones derived from TEI notes need to cling, but:
    clingyElements = Set("a"),
    // Note: empty elements are mis-processed by the browser (next element gets inserted inside the empty one!),
    // so I make sure there are no empty elements in the HTML:
    allowEmptyElements = false,
    // ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    // and in general, it's weird to expand the elements that are always empty:
    keepEmptyElements = Set("br", "meta", "link", "img", "data")
  )

  def read(rootPath: String): Site =
    read(new File(rootPath).toURI.toURL)

  def read(rootUrl: URL): Site = {
    logger.info(s"Reading site from $rootUrl")
    Parser.parseDo(Site.parse(Files.fileInDirectory(rootUrl, "site.xml")))
  }

  def main(args: Array[String]): Unit = {
    org.opentorah.collector.Cache.logEnabled = false

    Site
      .read(new File(args(0)).toURI.toURL)
      .build(args.length > 1 && (args(1) == "prettyPrint"))
  }

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
