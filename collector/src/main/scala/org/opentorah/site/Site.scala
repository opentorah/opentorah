package org.opentorah.site

import org.opentorah.metadata.Names
import org.opentorah.store.{Context, Directory, Path, Pure, Store, Stores}
import org.opentorah.tei.{Availability, LangUsage, Language, ProfileDesc, PublicationStmt, Publisher, Tei}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Caching, Doctype, Element, From, Html, Parser, PrettyPrinter, Sax, ScalaXml, Xml}
import org.slf4j.{Logger, LoggerFactory}
import zio.{Task, ZIO, ZLayer}
import java.net.URL

// TODO add static site server/generator.
// TODO consolidate all js, css etc. files in `asset` (singular)
// TODO fix favicon to the default `favicon.ico` and convert the Alter Rebbe picture.
abstract class Site(
  override val fromUrl: Element.FromUrl,
  val common: SiteCommon
) extends Context, Pure[Store], Element.FromUrl.With:

  final override def names: Names = common.names

  final def logger: Logger = Site.logger

  final val caching: Caching.Simple = new Caching.Simple

  final def toTask[T](parser: Caching.Parser[T]): Task[T] = Parser.toTask(Caching.provide(caching, parser))

  final private val staticPaths: Set[String] =
    Set("assets", "css", "js", "sass", "robots.txt") ++ common.getHtml.favicon.toSet

  final def isStatic(path: Seq[String]): Boolean =
    common.isStatic.contains(true) || path.headOption.exists(staticPaths.contains)

  final def getResponse(pathString: String): Task[Site.Response] =
    getResponse(Files.splitAndDecodeUrl(pathString))

  final def getResponse(path: Seq[String]): Task[Site.Response] = toTask(
    resolveRaw(path).flatMap((pathAndExtension: Site.PathAndExtension) =>
      content(pathAndExtension.path, pathAndExtension.extension))
  )

  protected val allowedExtensions: Set[String] = Set("html", "xml")

  // TODO verify the extension?
  def resolveUrl(url: String): Caching.Parser[Option[Path]] =
    resolveUrl(Files.splitAndDecodeUrl(url))

  def resolveUrl(url: Seq[String]): Caching.Parser[Option[Path]] =
    resolveRaw(url).map((pathAndExtension: Site.PathAndExtension) => Some(pathAndExtension.path)).orElse(ZIO.none)

  // Path returned is nonEmpty.
  private def resolveRaw(pathRaw: Seq[String]): Caching.Parser[Site.PathAndExtension] =
    val (path: Seq[String], mustBeNonTerminal: Boolean, extension: Option[String]) =
      if pathRaw.isEmpty then (pathRaw, false, None) else
        val last: String = pathRaw.last
        val (name: String, extension: Option[String]) = Files.nameAndExtension(last)

        val (indexCandidate: String, extensionRequested: Option[String]) =
          if !extension.exists(allowedExtensions.contains) then
            (last, None)
          else
            (name, extension)

        val (path: Seq[String], mustBeNonTerminal: Boolean) =
          if indexCandidate == "index"
          then (pathRaw.init                  , true )
          else (pathRaw.init :+ indexCandidate, false)

        (path, mustBeNonTerminal, extensionRequested)

    val result: Caching.Parser[Path] =
      if path.isEmpty && index.nonEmpty then ZIO.succeed(index.get) else for
        _ <- if initializeResolveDone then Effects.ok else
          initializeResolveDone = true
          initializeResolve
        result <- resolve(path)
      yield result

    result.flatMap((path: Path) =>
      if mustBeNonTerminal && !path.last.isInstanceOf[Stores[?]]
      then Effects.fail(s"Can not get an index of ${path.last}")
      else ZIO.succeed(Site.PathAndExtension(path, extension))
    )

  def pathShortener: Caching.Parser[Path.Shortener]

  private var initializeResolveDone: Boolean = false

  protected def initializeResolve: Caching.Parser[Unit] = Effects.ok

  protected def index: Option[Path] = None

  protected def content(path: Path, extension: Option[String]): Caching.Parser[Site.Response]

  final def render(path: Path): Caching.Parser[String] = for
    siteNavigationLinks: Seq[ScalaXml.Element] <- getNavigationLinks
    store: Store = path.last
    storeNavigationLinks: Seq[ScalaXml.Element] <- store.navigationLinks(path, this)
    content: ScalaXml.Element <- resolveLinks(path)
  yield
    val result: ScalaXml.Element = HtmlTheme.toHtml(
      siteHtml = common.getHtml,
      headTitle = store.htmlHeadTitle,
      cssFileName = store.style,
      viewer = store.viewer,
      navigationLinks = siteNavigationLinks ++ storeNavigationLinks,
      content = content
    )
    Site.prettyPrinter.render(ScalaXml, doctype = Some(Html))(result)

  final protected def resolveLinks(path: Path): Caching.Parser[ScalaXml.Element] = for
    pathShortener: Path.Shortener <- pathShortener
    store = path.last
    header: Option[ScalaXml.Element] <- store.header(path, this)
    content: ScalaXml.Element <- store.content(path, this)
    fullContent: ScalaXml.Element = HtmlTheme.fullContent(
      store.wrapperCssClass,
      header,
      store.htmlBodyTitle,
      content
    )
    result: ScalaXml.Element <- TeiToHtml.toHtml(fullContent).provideLayer(ZLayer.succeed(linkResolver(path, pathShortener)))
  yield result

  protected def linkResolver(
    path: Path,
    pathShortener: Path.Shortener
  ): LinksResolver

  final protected def renderTei(tei: Tei): String = Tei.prettyPrinter.renderWithHeader(ScalaXml)(
    Tei.xmlElement(tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = Some(PublicationStmt(
          publisher = common.getHtml.url.map(url => Publisher.Value(ScalaXml.toNodes(<ptr target={s"http://$url"}/>))),
          availability = Some(Availability(
            status = Some("free"),
            xml = ScalaXml.toNodes(ScalaXml.optional(common.license)(license =>
              <licence><ab><ref n="license" target={license.url}>{license.name}</ref></ab></licence>))
          ))
        )),
        sourceDesc = common.getTei.sourceDesc
      ),
      profileDesc = Some(tei.teiHeader.profileDesc.getOrElse(ProfileDesc.empty).copy(
        langUsage = Some(LangUsage(languages = Seq(Language(
          ident = tei.text.lang.get,
          usage = None,
          text = None
        )))),
        calendarDesc = common.getTei.calendarDesc
      ))
    )))
  )

  private def getNavigationLinks: Caching.Parser[Seq[ScalaXml.Element]] = caching.getCached("navigationLibks",
    ZIO.foreach(common.pages)(url => for
      pathOpt: Option[Path] <- resolveUrl(url)
      path: Path = pathOpt.get
      pathShortener: Path.Shortener <- pathShortener
    yield
      Path.a(path, pathShortener)(text = path.last.htmlHeadTitle.getOrElse("NO TITLE"))
    )
  )

  final def build(withPrettyPrint: Boolean): Task[Unit] = toTask {
    caching.logEnabled = false

    for
      _ <- writeDirectories
      _ <- innerBuild
      _ <- verify
      _ <- if !withPrettyPrint then Effects.ok else prettyPrint
    yield ()
  }

  private def writeDirectories: Caching.Parser[Unit] =
    logger.info("Writing site lists.")

    for
      directoriesToWrite: Seq[Directory[?, ?, ?]] <- directoriesToWrite
      _ <- ZIO.foreachDiscard(directoriesToWrite)(_.writeDirectory())
    yield ()

  protected def directoriesToWrite: Caching.Parser[Seq[Directory[?, ?, ?]]] = ZIO.succeed(Seq.empty)

  protected def innerBuild: Caching.Parser[Unit] = Effects.ok

  protected def verify: Caching.Parser[Unit] = Effects.ok

  private def prettyPrint: Caching.Parser[Unit] =
    logger.info("Pretty-printing site.")

    val xml: Xml = ScalaXml

    for
      prettyPrintTei    : Seq[URL]  <- prettyPrintTei
      prettyPrintStores : Seq[URL]  <- prettyPrintStores
    yield
      prettyPrint(prettyPrintTei   , Tei.prettyPrinter      , doctype = None, xml)
      prettyPrint(prettyPrintStores, Site.storePrettyPrinter, doctype = None, xml)

  private def prettyPrint(urls: Seq[URL], prettyPrinter: PrettyPrinter, doctype: Option[Doctype], xml: Xml): Unit =
    for url <- urls do Files.write(
      file = Files.url2file(url),
      content = prettyPrinter.renderWithHeader(xml, doctype)(Effects.unsafeRun(From.url(url, xml).load.map(_.asInstanceOf[xml.Element])))
    )

  protected def prettyPrintTei: Caching.Parser[Seq[URL]] = ZIO.succeed(Seq.empty)

  protected def prettyPrintStores: Caching.Parser[Seq[URL]] = ZIO.succeed(Seq.empty)

object Site:

  final val logger: Logger = LoggerFactory.getLogger(this.getClass)

  final class PathAndExtension(
    val path: Path,
    val extension: Option[String]
  )

  final class Response(
    val content: String,
    val mimeType: String
  )

  object Navigation:
    val prev: String = "⇦"
    val next: String = "⇨"
    val up  : String = "⇧"
    val text: String = "A"

  val storePrettyPrinter: PrettyPrinter = PrettyPrinter(
    nestElements = Set("p"), // TODO remnants of TEI?
    alwaysStackElements = Set("store", "by")
  )

  val prettyPrinter: PrettyPrinter = Html.prettyPrinter + Tei.prettyPrinter
