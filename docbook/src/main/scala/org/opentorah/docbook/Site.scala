package org.opentorah.docbook

import org.opentorah.html
import org.opentorah.site.{Caching, Store, Viewer, Site => HtmlSite}
import org.opentorah.util.Files
import org.opentorah.xml._
import zio.ZIO
import java.io.File

// TODO document eventually
final class Site(
  title: HtmlSite.Title.Value,
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
  footer: HtmlSite.Footer.Value,
) extends HtmlSite[Site](
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
  Site.defaultViewer // TODO turn into method and override?
) {
  override protected def resolveNavigationalLink(url: String): Caching.Parser[Xml.Element] =
    ZIO.succeed(html.a(Seq(url))(text = url)) // TODO

  override protected def resolveHtmlContent(htmlContent: org.opentorah.site.HtmlContent[Site]): Caching.Parser[Xml.Element] =
    htmlContent.content(this) >>= (content => DocBook.toHtml0(content))

  override protected def htmlPrettyPrinter: PrettyPrinter = Site.htmlPrettyPrinter

  override def a(path: Store.Path): html.a = ???

  override def resolve(path: Seq[String]): Caching.Parser[Option[Store.Path]] = ???
}

object Site extends Element[Site]("site") {
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

  def read(file: File): Site = Parser.run(Site.parse(Files.file2url(file)))

  override def contentParsable: Parsable[Site] = new Parsable[Site] {
    override def parser: Parser[Site] = for {
      title <- HtmlSite.Title.element.required()
      siteUrl <- HtmlSite.siteUrlAttribute()
      favicon <- HtmlSite.faviconAttribute()
      pages <- HtmlSite.Page.seq()
      licenseName <- HtmlSite.licenseNameAttribute()
      licenseUrl <- HtmlSite.licenseUrlAttribute()
      googleAnalyticsId <- HtmlSite.googleAnalyticsIdAttribute()
      isMathJaxEnabled <- HtmlSite.isMathJaxEnabledAttribute()
      useMathJax3 <- HtmlSite.useMathJax3Attribute()
      email <- HtmlSite.emailAttribute()
      githubUsername <- HtmlSite.githubUsernameAttribute()
      twitterUsername <- HtmlSite.twitterUsernameAttribute()
      footer <- HtmlSite.Footer.element.required()
    } yield new Site(
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

    override def unparser: Unparser[Site] = Unparser.concat[Site](
      HtmlSite.Title.element.required(_.title),
      HtmlSite.siteUrlAttribute(_.siteUrl),
      HtmlSite.faviconAttribute(_.favicon),
      HtmlSite.Page.seq(_.pages),
      HtmlSite.licenseNameAttribute(_.licenseName),
      HtmlSite.licenseUrlAttribute(_.licenseUrl),
      HtmlSite.googleAnalyticsIdAttribute(_.googleAnalyticsId),
      HtmlSite.isMathJaxEnabledAttribute(_.isMathJaxEnabled),
      HtmlSite.useMathJax3Attribute(_.useMathJax3),
      HtmlSite.emailAttribute(_.email),
      HtmlSite.githubUsernameAttribute(_.githubUsername),
      HtmlSite.twitterUsernameAttribute(_.twitterUsername),
      HtmlSite.Footer.element.required(_.footer),
    )
  }
}
