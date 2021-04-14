package org.opentorah.docbook

import org.opentorah.html
import org.opentorah.site.{Caching, HtmlContent, Site, Store, Viewer}
import org.opentorah.util.Files
import org.opentorah.xml.{Element, Parsable, Parser, PrettyPrinter, Unparser, Xml}
import zio.{ZIO, ZLayer}
import java.io.File

// TODO document eventually
final class DocBookSite(
  title: Site.Title.Value,
  siteUrl: String,
  favicon: String,
  pages: Seq[String],
  licenseName: String,
  licenseUrl: String,
  googleAnalyticsId: Option[String],
  isMathJaxEnabled: Boolean,
  useMathJax3: Boolean,
  email: String,
  githubUsername: Option[String],
  twitterUsername: Option[String],
  footer: Site.Footer.Value,
) extends Site[DocBookSite](
  title,
  siteUrl,
  favicon,
  pages,
  licenseName,
  licenseUrl,
  googleAnalyticsId,
  isMathJaxEnabled,
  useMathJax3,
  email,
  githubUsername,
  twitterUsername,
  footer,
  DocBookSite.defaultViewer // TODO turn into method and override?
) {
  override protected def resolveNavigationalLink(url: String): Caching.Parser[Xml.Element] =
    ZIO.succeed(html.a(Seq(url))(text = url)) // TODO

  override protected def resolveHtmlContent(htmlContent: org.opentorah.site.HtmlContent[DocBookSite]): Caching.Parser[Xml.Element] =
    htmlContent.content(this) >>= (content => DocBook.toHtml(content).provideLayer(ZLayer.succeed(())))

  override protected def htmlPrettyPrinter: PrettyPrinter = DocBookSite.htmlPrettyPrinter

  override def resolve(path: Seq[String]): Caching.Parser[Option[Store.Path]] = ???

  override def style(htmlContent: HtmlContent[DocBookSite]): String = "main"

  override def viewer(htmlContent: HtmlContent[DocBookSite]): org.opentorah.site.Viewer = DocBookSite.defaultViewer

  override def path(htmlContent: HtmlContent[DocBookSite]): Store.Path = ???

  def navigationLinks(htmlContent: HtmlContent[DocBookSite]): Caching.Parser[Seq[Xml.Element]] = htmlContent match {
    case _ => ZIO.succeed (Seq.empty)
  }
}

object DocBookSite extends Element[DocBookSite]("site") {
   val defaultViewer: Viewer = new Viewer("defaultViewer"){}

  // TODO unify with the one in the Collector:
  val htmlPrettyPrinter: PrettyPrinter = DocBook.prettyPrinter.copy(
    alwaysStackElements = DocBook.prettyPrinter.alwaysStackElements ++ Set("nav", "header", "main", "div"),
    // Note: only the ones derived from DocBook notes need to cling, but:
    clingyElements = DocBook.prettyPrinter.clingyElements ++ Set("a"), // TODO
    allowEmptyElements = false,
    // ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    // and in general, it's weird to expand the elements that are always empty:
    keepEmptyElements = Set("br", "meta", "link", "img", "data"),
    preformattedElements = Set("pre")
  )

  def read(file: File): DocBookSite = Parser.run(DocBookSite.parse(Files.file2url(file)))

  override def contentParsable: Parsable[DocBookSite] = new Parsable[DocBookSite] {
    override def parser: Parser[DocBookSite] = for {
      title <- Site.Title.element.required()
      siteUrl <- Site.siteUrlAttribute()
      favicon <- Site.faviconAttribute()
      pages <- Site.Page.seq()
      licenseName <- Site.licenseNameAttribute()
      licenseUrl <- Site.licenseUrlAttribute()
      googleAnalyticsId <- Site.googleAnalyticsIdAttribute()
      isMathJaxEnabled <- Site.isMathJaxEnabledAttribute()
      useMathJax3 <- Site.useMathJax3Attribute()
      email <- Site.emailAttribute()
      githubUsername <- Site.githubUsernameAttribute()
      twitterUsername <- Site.twitterUsernameAttribute()
      footer <- Site.Footer.element.required()
    } yield new DocBookSite(
      title,
      siteUrl,
      favicon,
      pages,
      licenseName,
      licenseUrl,
      googleAnalyticsId,
      isMathJaxEnabled,
      useMathJax3,
      email,
      githubUsername,
      twitterUsername,
      footer
    )

    override def unparser: Unparser[DocBookSite] = Unparser.concat[DocBookSite](
      Site.Title.element.required(_.title),
      Site.siteUrlAttribute(_.siteUrl),
      Site.faviconAttribute(_.favicon),
      Site.Page.seq(_.pages),
      Site.licenseNameAttribute(_.licenseName),
      Site.licenseUrlAttribute(_.licenseUrl),
      Site.googleAnalyticsIdAttribute(_.googleAnalyticsId),
      Site.isMathJaxEnabledAttribute(_.isMathJaxEnabled),
      Site.useMathJax3Attribute(_.useMathJax3),
      Site.emailAttribute(_.email),
      Site.githubUsernameAttribute(_.githubUsername),
      Site.twitterUsernameAttribute(_.twitterUsername),
      Site.Footer.element.required(_.footer),
    )
  }
}
