package org.opentorah.site

import org.opentorah.docbook.DocBook
import org.opentorah.html
import org.opentorah.store.{Caching, Directory, Store, Stores}
import org.opentorah.tei.{Availability, LangUsage, Language, LinksResolver, ProfileDesc, PublicationStmt, Publisher, Tei}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Doctype, Element, Parser, PrettyPrinter, ScalaXml, Xml}
import org.slf4j.{Logger, LoggerFactory}
import zio.{IO, Task, ZIO, ZLayer}
import java.net.URL

// TODO add static site server/generator.
// TODO consolidate all js, css etc. files in `asset` (singular)
// TODO fix favicon to the default `favicon.ico` and convert the Alter Rebbe picture.
abstract class Site[S <: Site[S]](
  override val fromUrl: Element.FromUrl,
  val common: SiteCommon
) extends Stores.Pure, Element.FromUrl.With { this: S =>

  final def logger: Logger = Site.logger

  def defaultViewer: Option[Viewer] = None

  final val caching: Caching.Simple = new Caching.Simple

  final protected def toTask[T](parser: Caching.Parser[T]): Task[T] = Parser.toTask(Caching.provide(caching, parser))

  final private val staticPaths: Set[String] =
    Set("assets", "css", "js", "sass", "robots.txt") ++
    common.favicon.toSet

  final def isStatic(path: Seq[String]): Boolean = path.headOption.exists(staticPaths.contains)

  final def getResponse(pathString: String): Task[Site.Response] =
    getResponse(Files.splitAndDecodeUrl(pathString))

  final def getResponse(path: Seq[String]): Task[Site.Response] = toTask(
    resolve(path).flatMap {
      case Site.PathAndExtension(path, extension) => content(path.last, extension)
    }
  )

  protected val allowedExtensions: Set[String] = Set("html", "xml")

  final def resolveUrl(url: String): Caching.Parser[Option[Store.Path]] =
    resolveUrl(Files.splitAndDecodeUrl(url))

  // TODO verify the extension?
  def resolveUrl(url: Seq[String]): Caching.Parser[Option[Store.Path]] =
    resolve(url).map {
      case Site.PathAndExtension(path, extension) => Some(path)
    }.orElse(ZIO.none)

  // Store.Path returned is nonEmpty.
  final protected def resolve(pathRaw: Seq[String]): Caching.Parser[Site.PathAndExtension] =
    val (path: Seq[String], mustBeNonTerminal: Boolean, extension: Option[String]) =
      if pathRaw.isEmpty then (pathRaw, false, None) else
        val last: String = pathRaw.last
        val (name: String, extension: Option[String]) = Files.nameAndExtension(last)

        val (indexCandidate: String, extensionRequested: Option[String]) =
          if !extension.exists(allowedExtensions.contains) then
            (last, None)
          else
            (name, extension)

        if indexCandidate == "index" then
          (pathRaw.init                  , true , extensionRequested)
        else
          (pathRaw.init :+ indexCandidate, false, extensionRequested)

    val result: Caching.Parser[Store.Path] =
      if path.isEmpty && index.nonEmpty then ZIO.succeed(index.get) else
        (if initializeResolveDone then Effects.ok else
          initializeResolveDone = true
          initializeResolve
        ) *>
          Stores.resolve(path, this)

    result.flatMap((path: Store.Path) =>
      if mustBeNonTerminal && !path.last.isInstanceOf[Store.NonTerminal] then
        Effects.fail(s"Can not get an index of ${path.last}")
      else
        ZIO.succeed(Site.PathAndExtension(path, extension))
    )

  private var initializeResolveDone: Boolean = false

  protected def initializeResolve: Caching.Parser[Unit] = Effects.ok

  protected def index: Option[Store.Path] = None

  protected def content(store: Store, extension: Option[String]): Caching.Parser[Site.Response] = ???

  final def renderHtmlContent(htmlContent: HtmlContent[S]): Caching.Parser[String] = for
    content: ScalaXml.Element <- resolveLinksInHtmlContent(htmlContent)
    siteNavigationLinks: Seq[ScalaXml.Element] <- getNavigationLinks
    htmlContentNavigationLinks: Seq[ScalaXml.Element] <- navigationLinks(htmlContent)
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

  private var navigationLinks: Option[Seq[ScalaXml.Element]] = None

  private def getNavigationLinks: Caching.Parser[Seq[ScalaXml.Element]] =
    if navigationLinks.isDefined then ZIO.succeed(navigationLinks.get) else
      ZIO.foreach(common.pages)(resolveNavigationalLink).map(result =>
        navigationLinks = Some(result)
        result
      )

  private def resolveNavigationalLink(url: String): Caching.Parser[ScalaXml.Element] = // TODO html.a!
    resolveUrl(url).map(pathOpt =>
      val path: Store.Path = pathOpt.get
      a(path)(text = path.last.asInstanceOf[HtmlContent[S]].htmlHeadTitle.getOrElse("NO TITLE"))
    )

  final protected def resolveLinksInHtmlContent(htmlContent: HtmlContent[S]): Caching.Parser[ScalaXml.Element] =
    htmlContent.content(this).flatMap(content =>
      if ScalaXml.getNamespace(content) == DocBook.namespace.default then
        DocBook.toHtml(content).provideLayer(ZLayer.succeed(()))
      else
        Tei    .toHtml(content).provideLayer(ZLayer.succeed(linkResolver(htmlContent)))
    )

  protected def linkResolver(htmlContent: HtmlContent[S]): LinksResolver = LinksResolver.empty

  final def a(htmlContent: HtmlContent[S]): html.a = a(path(htmlContent))

  final def a(path: Store.Path): html.a = html
    .a(path.map(_.structureName))
    .setTarget((path.last match
      case htmlContent: HtmlContent[?] => viewer(htmlContent.asInstanceOf[HtmlContent[S]])
      case _ => defaultViewer
    ).map(_.name))

  protected def path(htmlContent: HtmlContent[S]): Store.Path = ???

  def style(htmlContent: HtmlContent[S]): String = "main"

  def viewer(htmlContent: HtmlContent[S]): Option[Viewer] = defaultViewer

  // TODO split into prev, next, up and more?
  protected def navigationLinks(htmlContent: HtmlContent[S]): Caching.Parser[Seq[ScalaXml.Element]] = ZIO.succeed (Seq.empty)

  final def build(withPrettyPrint: Boolean): Task[Unit] = toTask {
    caching.logEnabled = false

    (if !withPrettyPrint then Effects.ok else Effects.effect(prettyPrint(ScalaXml))) *>
    Effects.effect(Site.logger.info("Writing site lists.")) *>
    ZIO.foreach_(directoriesToWrite)(_.writeDirectory()) *>
    ZIO.foreach_(common.docbook)(docbook => ZIO.succeed(docbook.process(this))) *>
    buildMore
  }

  final private def prettyPrint(xml: Xml): Unit =
    Site.logger.info("Pretty-printing site.")
    PrettyPrinter.prettyPrint(
      roots = prettyPrintRoots.toList.map(Files.url2file),
      xml,
      recognizer = new PrettyPrinter.Recognizer:
        override def recognize(model: Xml)(element: model.Element): (PrettyPrinter, Option[Doctype]) =
          if model.getName(element) == "TEI" then
            (Tei.prettyPrinter, None)
          else
            (Site.storePrettyPrinter, None)
    )

  protected def prettyPrintRoots: Seq[URL] = Seq.empty

  protected def directoriesToWrite: Seq[Directory[?, ?, ?]] = Seq.empty

  protected def buildMore: Caching.Parser[Unit] = IO.succeed(())
}

object Site:

  final val logger: Logger = LoggerFactory.getLogger(this.getClass)

  final case class PathAndExtension(
    path: Store.Path,
    extension: Option[String]
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
  ), Stores.Pure:
    override def storesPure: Seq[Store] = Seq.empty
