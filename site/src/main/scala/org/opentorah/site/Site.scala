package org.opentorah.site

import org.opentorah.docbook.DocBook
import org.opentorah.html
import org.opentorah.store.{Caching, Directory, FindByName, Store}
import org.opentorah.tei.{Availability, LangUsage, Language, LinksResolver, ProfileDesc, PublicationStmt, Publisher, Tei}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Doctype, FromUrl, Model, Parser, PrettyPrinter, Xml}
import org.slf4j.{Logger, LoggerFactory}
import zio.{IO, Task, ZIO, ZLayer}
import java.net.URL

// TODO add static site server/generator.
// TODO consolidate all js, css etc. files in `asset` (singular)
// TODO fix favicon to the default `favicon.ico` and convert the Alter Rebbe picture.
class Site[S <: Site[S]](
  override val fromUrl: FromUrl,
  val common: SiteCommon
) extends FindByName with FromUrl.With { this: S =>

  final def logger: Logger = Site.logger

  def defaultViewer: Option[Viewer] = None

  final val caching: Caching.Simple = new Caching.Simple

  final protected def toTask[T](parser: Caching.Parser[T]): Task[T] = Parser.toTask(Caching.provide(caching, parser))

  final private val staticPaths: Set[String] =
    Set("assets", "css", "js", "sass", "robots.txt") ++
    common.favicon.toSet

  final def resolveContent(path: Seq[String]): Task[Option[Site.Response]] =
    if (path.headOption.exists(staticPaths.contains)) Task.none
    else toTask(resolve(path) >>= (_.map(content(_).map(Some(_))).getOrElse(ZIO.none)))

  final def resolve(url: String): Caching.Parser[Option[Store.Path]] = resolve(Files.splitAndDecodeUrl(url))

  final protected def resolve(path: Seq[String]): Caching.Parser[Option[Store.Path]] =
    if (path.isEmpty) index else resolve(path, Seq.empty)

  protected def index: Caching.Parser[Option[Store.Path]] = ZIO.none

  protected def content(path: Store.Path): Caching.Parser[Site.Response] = ???

  final def renderHtmlContent(htmlContent: HtmlContent[S]): Caching.Parser[String] = for {
    content <- resolveHtmlContent(htmlContent)
    siteNavigationLinks <- getNavigationLinks
    htmlContentNavigationLinks <- navigationLinks(htmlContent)
  } yield Site.prettyPrinter.render(doctype = html.Html, element = HtmlTheme.toHtml(
      htmlContent,
      navigationLinks = siteNavigationLinks ++ htmlContentNavigationLinks,
      content = content,
      site = this
    ))

  final protected def renderTeiContent(tei: Tei): String = Tei.prettyPrinter.renderXml(Tei.xmlElement(tei.copy(teiHeader = tei.teiHeader.copy(
    fileDesc = tei.teiHeader.fileDesc.copy(
      publicationStmt = Some(new PublicationStmt(
        publisher = common.url.map(url => new Publisher.Value(<ptr target={s"http://$url"}/>)),
        availability = Some(new Availability(
          status = Some("free"),
          xml = Xml.optional(common.license)(license => <licence><ab><ref n="license" target={license.url}>{license.name}</ref></ab></licence>)
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
  ))))

  private var navigationLinks: Option[Seq[Xml.Element]] = None

  private def getNavigationLinks: Caching.Parser[Seq[Xml.Element]] =
    if (navigationLinks.isDefined) ZIO.succeed(navigationLinks.get) else
      ZIO.foreach(common.pages)(resolveNavigationalLink).map { result =>
        navigationLinks = Some(result)
        result
      }

  private def resolveNavigationalLink(url: String): Caching.Parser[Xml.Element] = // TODO html.a!
    resolve(url).map { pathOpt =>
      val path: Store.Path = pathOpt.get
      a(path)(text = path.last.asInstanceOf[HtmlContent[S]].htmlHeadTitle.getOrElse("NO TITLE"))
    }

  final protected def resolveHtmlContent(htmlContent: HtmlContent[S]): Caching.Parser[Xml.Element] =
    htmlContent.content(this) >>= { content =>
      if (Xml.getNamespace(content) == DocBook.namespace.default)
        DocBook.toHtml(content).provideLayer(ZLayer.succeed(()))
      else
        Tei    .toHtml(content).provideLayer(ZLayer.succeed(linkResolver(htmlContent)))
    }

  protected def linkResolver(htmlContent: HtmlContent[S]): LinksResolver = LinksResolver.empty

  final def a(htmlContent: HtmlContent[S]): html.a = a(path(htmlContent))

  final def a(path: Store.Path): html.a = html
    .a(path.map(_.structureName))
    .setTarget((path.last match {
      case htmlContent: HtmlContent[S] => viewer(htmlContent)
      case _ => defaultViewer
    }).map(_.name))

  protected def path(htmlContent: HtmlContent[S]): Store.Path = ???

  def style(htmlContent: HtmlContent[S]): String = "main"

  def viewer(htmlContent: HtmlContent[S]): Option[Viewer] = defaultViewer

  // TODO split into prev, next, up and more?
  protected def navigationLinks(htmlContent: HtmlContent[S]): Caching.Parser[Seq[Xml.Element]] = ZIO.succeed (Seq.empty)

  final def build(withPrettyPrint: Boolean): Task[Unit] = toTask {
    caching.logEnabled = false

    (if (!withPrettyPrint) Effects.ok else Effects.effect(prettyPrint(Xml))) *>
    Effects.effect(Site.logger.info("Writing site lists.")) *>
    ZIO.foreach_(directoriesToWrite)(_.writeDirectory()) *>
    ZIO.foreach_(common.docbook)(docbook => ZIO.succeed(docbook.process(this))) *>
    buildMore
  }

  final private def prettyPrint(model: Model): Unit = {
    Site.logger.info("Pretty-printing site.")
    PrettyPrinter.prettyPrint(
      roots = prettyPrintRoots.toList.map(Files.url2file),
      model,
      recognizer = new PrettyPrinter.Recognizer {
        override def recognize(model: Model)(element: model.Element): (PrettyPrinter, Option[Doctype]) =
          if (model.getName(element) == "TEI")
            (Tei.prettyPrinter, None)
          else
            (Store.prettyPrinter, None)
      }
    )
  }

  protected def prettyPrintRoots: Seq[URL] = Seq.empty

  protected def directoriesToWrite: Seq[Directory[_, _, _]] = Seq.empty

  protected def buildMore: Caching.Parser[Unit] = IO.succeed(())
}

object Site {

  final val logger: Logger = LoggerFactory.getLogger(this.getClass)

  final class Response(
    val content: String,
    val mimeType: String
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
    fromUrl: FromUrl,
    common: SiteCommon
  ) extends Site[Common](
    fromUrl,
    common
  )
}
