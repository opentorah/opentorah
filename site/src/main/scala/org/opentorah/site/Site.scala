package org.opentorah.site

import org.opentorah.docbook.DocBook
import org.opentorah.html
import org.opentorah.metadata.Names
import org.opentorah.store.{Directory, Path, Pure, Store, Stores}
import org.opentorah.tei.{Availability, LangUsage, Language, LinksResolver, ProfileDesc, PublicationStmt, Publisher, Tei}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Caching, Doctype, Element, Parser, PrettyPrinter, ScalaXml, Xml}
import org.slf4j.{Logger, LoggerFactory}
import zio.{IO, Task, ZIO, ZLayer}
import java.io.File
import java.net.URL

// TODO add static site server/generator.
// TODO consolidate all js, css etc. files in `asset` (singular)
// TODO fix favicon to the default `favicon.ico` and convert the Alter Rebbe picture.
abstract class Site[S <: Site[S]](
  override val fromUrl: Element.FromUrl,
  val common: SiteCommon
) extends Pure[Store], Element.FromUrl.With:
  this: S =>

  final override def names: Names = common.names

  final def logger: Logger = Site.logger

  final val caching: Caching.Simple = new Caching.Simple

  final protected def toTask[T](parser: Caching.Parser[T]): Task[T] = Parser.toTask(Caching.provide(caching, parser))

  final private val staticPaths: Set[String] =
    Set("assets", "css", "js", "sass", "robots.txt") ++ common.favicon.toSet

  final def isStatic(path: Seq[String]): Boolean = path.headOption.exists(staticPaths.contains)

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

  // Store.Path returned is nonEmpty.
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
          if indexCandidate == "index" then
            (pathRaw.init                  , true )
          else
            (pathRaw.init :+ indexCandidate, false)

        (path, mustBeNonTerminal, extensionRequested)

    val result: Caching.Parser[Path] =
      if path.isEmpty && index.nonEmpty then ZIO.succeed(index.get) else for
        _ <- if initializeResolveDone then Effects.ok else
          initializeResolveDone = true
          initializeResolve
        result <- resolve(path)
      yield result

    result.flatMap((path: Path) =>
      if mustBeNonTerminal && !path.last.isInstanceOf[Stores[?]] then
        Effects.fail(s"Can not get an index of ${path.last}")
      else
        ZIO.succeed(Site.PathAndExtension(path, extension))
    )

  final def a(path: Path): html.a = html
    .a(Path.structureNames(shortenPath(path)))
    .setTarget(path.last.asInstanceOf[HtmlContent[?]].viewer)

  protected def shortenPath(path: Path): Path = path

  private var initializeResolveDone: Boolean = false

  protected def initializeResolve: Caching.Parser[Unit] = Effects.ok

  protected def index: Option[Path] = None

  protected def content(path: Path, extension: Option[String]): Caching.Parser[Site.Response]

  final def renderHtmlContent(path: Path, htmlContent: HtmlContent[S]): Caching.Parser[String] = for
    content: ScalaXml.Element <- resolveLinksInHtmlContent(path, htmlContent)
    siteNavigationLinks: Seq[ScalaXml.Element] <- getNavigationLinks
    htmlContentNavigationLinks: Seq[ScalaXml.Element] <- htmlContent.navigationLinks(path, this)
  yield Site.prettyPrinter.render(ScalaXml, doctype = Some(html.Html))(HtmlTheme.toHtml(
    htmlContent,
    navigationLinks = siteNavigationLinks ++ htmlContentNavigationLinks,
    content = content,
    site = this
  ))

  final protected def renderTeiContent(tei: Tei): String = Tei.prettyPrinter.renderWithHeader(ScalaXml)(
    Tei.xmlElement(tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = Some(PublicationStmt(
          publisher = common.url.map(url => Publisher.Value(ScalaXml.toNodes(<ptr target={s"http://$url"}/>))),
          availability = Some(Availability(
            status = Some("free"),
            xml = ScalaXml.toNodes(ScalaXml.optional(common.license)(license => <licence><ab><ref n="license" target={license.url}>{license.name}</ref></ab></licence>))
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

  // TODO use the cache:
  private var navigationLinks: Option[Seq[ScalaXml.Element]] = None
  private def getNavigationLinks: Caching.Parser[Seq[ScalaXml.Element]] =
    if navigationLinks.isDefined then ZIO.succeed(navigationLinks.get) else
      ZIO.foreach(common.pages)(resolveNavigationalLink).map(result =>
        navigationLinks = Some(result)
        result
      )

  private def resolveNavigationalLink(url: String): Caching.Parser[ScalaXml.Element] = // TODO html.a!
    resolveUrl(url).map((pathOpt: Option[Path]) =>
      val path: Path = pathOpt.get
      a(path)(text = path.last.asInstanceOf[HtmlContent[S]].htmlHeadTitle.getOrElse("NO TITLE"))
    )

  final protected def resolveLinksInHtmlContent(
    path: Path,
    htmlContent: HtmlContent[S]
  ): Caching.Parser[ScalaXml.Element] = for
    content <- htmlContent.content(path, this)
    result <-
      if ScalaXml.getNamespace(content) == DocBook.namespace.default then
        DocBook.toHtml(content).provideLayer(ZLayer.succeed(()))
      else
        Tei    .toHtml(content).provideLayer(ZLayer.succeed(linkResolver(path, htmlContent)))
  yield result

  protected def linkResolver(path: Path, htmlContent: HtmlContent[S]): LinksResolver = LinksResolver.empty

  final def build(withPrettyPrint: Boolean): Task[Unit] = toTask {
    caching.logEnabled = false

    for
      _ <- writeDirectories
      _ <- innerBuild
      _ <- verify
      _ <- processDocbook
      _ <- if !withPrettyPrint then Effects.ok else prettyPrint
    yield ()
  }

  private def writeDirectories: Caching.Parser[Unit] =
    logger.info("Writing site lists.")

    for
      directoriesToWrite: Seq[Directory[?, ?, ?]] <- directoriesToWrite
      _ <- ZIO.foreach_(directoriesToWrite)(_.writeDirectory())
    yield ()

  protected def directoriesToWrite: Caching.Parser[Seq[Directory[?, ?, ?]]] = ZIO.succeed(Seq.empty)

  protected def innerBuild: Caching.Parser[Unit] = Effects.ok

  protected def verify: Caching.Parser[Unit] = Effects.ok

  private def processDocbook: Caching.Parser[Unit] =
    ZIO.foreach_(common.docbook)(docbook => ZIO.succeed(docbook.process(this)))

  private def prettyPrint: Caching.Parser[Unit] =
    logger.info("Pretty-printing site.")

    // Note: Scala XML ignores XML comments when parsing a file
    // (see https://github.com/scala/scala-xml/issues/508);
    // use Dom to keep them...
    val xml: Xml = ScalaXml

    for
      prettyPrintTei <- prettyPrintTei
      prettyPrintStores <- prettyPrintStores
    yield
      for url <- prettyPrintTei    do prettyPrint(url, Tei.prettyPrinter      , doctype = None, xml)
      for url <- prettyPrintStores do prettyPrint(url, Site.storePrettyPrinter, doctype = None, xml)

  private def prettyPrint(url: URL, prettyPrinter: PrettyPrinter, doctype: Option[Doctype], xml: Xml): Unit =
    Files.write(
      file = Files.url2file(url),
      content = prettyPrinter.renderWithHeader(xml, doctype)(xml.loadFromUrl(url))
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

  val storePrettyPrinter: PrettyPrinter = PrettyPrinter(
    nestElements = Set("p"), // TODO remnants of TEI?
    alwaysStackElements = Set("store", "by")
  )

  val prettyPrinter: PrettyPrinter = PrettyPrinter(
    doNotStackElements =
      Tei.prettyPrinter.doNotStackElements ++
      DocBook.prettyPrinter.doNotStackElements,
    alwaysStackElements =
      Tei.prettyPrinter.alwaysStackElements ++
      DocBook.prettyPrinter.alwaysStackElements ++
      Set("nav", "header", "main", "div", "store"),
    nestElements =
      Tei.prettyPrinter.nestElements ++
      DocBook.prettyPrinter.nestElements,
    clingyElements =
      // Note: only the ones derived from TEI/DocBook notes need to cling, but:
      Tei.prettyPrinter.clingyElements ++
      DocBook.prettyPrinter.clingyElements ++
      Set("a"),
    allowEmptyElements = false,
    // ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    // and in general, it's weird to expand the elements that are always empty:
    keepEmptyElements = Set("br", "meta", "link", "img", "data"),
    preformattedElements = Set("pre")
  )

  final class Common(
    fromUrl: Element.FromUrl,
    common: SiteCommon
  ) extends Site[Common](
    fromUrl,
    common
  ), Pure[?]:
    override def storesPure: Seq[Store] = Seq.empty
    override def content(path: Path, extension: Option[String]): Caching.Parser[Response] = ???

