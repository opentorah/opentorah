package org.opentorah.site

import org.opentorah.html
import org.opentorah.mathjax.MathJaxConfiguration
import org.opentorah.xml.{Attribute, Element, Parsable, PrettyPrinter, RawXml, Xml}
import zio.ZIO

// TODO pretty-print all discovered files (TEI, DocBook...)
abstract class Site[S <: Site[S]](
  val title: Site.Title.Value,
  val siteUrl: String,
  val favicon: String,
  val pages: Seq[String],
  val licenseName: String,
  val licenseUrl: String,
  val googleAnalyticsId: Option[String],
  val isMathJaxEnabled: Boolean,
  val email: String,
  val githubUsername: Option[String],
  val twitterUsername: Option[String],
  val footer: Site.Footer.Value,
  val defaultViewer: Viewer
) { this: S =>

  val caching: Caching.Simple = new Caching.Simple

  // TODO
//  val mathJaxConfiguration: MathJaxConfiguration = MathJaxConfiguration(
//    font = mathJaxFont.get,
//    extensions = mathJaxExtensions.get.asScala.toList,
//    texDelimiters = MathJaxConfiguration.delimiters(texDelimiter.get),
//    texInlineDelimiters = MathJaxConfiguration.delimiters(texInlineDelimiter.get),
//    asciiMathDelimiters = MathJaxConfiguration.delimiters(asciiMathDelimiter.get),
//    processEscapes = processMathJaxEscapes.get
//  )

  def mathJaxConfiguration: MathJaxConfiguration = MathJaxConfiguration(
    extensions = List.empty,
    texDelimiters = MathJaxConfiguration.delimiters("$$"),
    texInlineDelimiters = MathJaxConfiguration.delimiters("$")
  )

  final def renderHtmlContent(htmlContent: HtmlContent[S]): Caching.Parser[String] = for {
    content <- resolveHtmlContent(htmlContent)
    siteNavigationLinks <- getNavigationLinks
    htmlContentNavigationLinks <- htmlContent.navigationLinks(this)
  } yield htmlPrettyPrinter.render(doctype = html.Html, element = HtmlTheme.toHtml(
    htmlContent,
    navigationLinks = siteNavigationLinks ++ htmlContentNavigationLinks,
    content = content,
    site = this
  ))

  private var navigationLinks: Option[Seq[Xml.Element]] = None

  final def getNavigationLinks: Caching.Parser[Seq[Xml.Element]] =
    if (navigationLinks.isDefined) ZIO.succeed(navigationLinks.get) else
      ZIO.foreach(pages)(resolveNavigationalLink).map { result =>
        navigationLinks = Some(result)
        result
      }

  protected def resolveNavigationalLink(url: String): Caching.Parser[Xml.Element] // TODO html.a!

  protected def resolveHtmlContent(htmlContent: HtmlContent[S]): Caching.Parser[Xml.Element]

  protected def htmlPrettyPrinter: PrettyPrinter
}

object Site {
  object Title  extends RawXml("title")
  object Footer extends RawXml("footer")

  object Page extends Element[String]("page") {
    private val urlAttribute: Attribute.Required[String] = Attribute("url").required
    override def contentParsable: Parsable[String] = urlAttribute
  }

  val siteUrlAttribute: Attribute.Required[String] = Attribute("siteUrl").required
  val faviconAttribute: Attribute.Required[String] = Attribute("favicon").required
  val licenseNameAttribute: Attribute.Required[String] = Attribute("licenseName").required
  val licenseUrlAttribute: Attribute.Required[String] = Attribute("licenseUrl").required
  val googleAnalyticsIdAttribute: Attribute.Optional[String] = Attribute("googleAnalyticsId").optional
  val isMathJaxEnabledAttribute: Attribute.OrDefault[Boolean] = new Attribute.BooleanAttribute("isMathJaxEnabled").orDefault
  val emailAttribute: Attribute.Required[String] = Attribute("email").required
  val githubUsernameAttribute: Attribute.Optional[String] = Attribute("githubUsername").optional
  val twitterUsernameAttribute: Attribute.Optional[String] = Attribute("twitterUsername").optional
}
