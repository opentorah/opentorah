package org.opentorah.site

import org.opentorah.html
import org.opentorah.mathjax.{Delimiters, MathJaxConfiguration}
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Doctype, Element, Parsable, PrettyPrinter, RawXml, Xml}
import zio.ZIO
import java.io.File
import scala.annotation.tailrec

// TODO pretty-print all discovered files (TEI, DocBook...) by the Site-determined PrettyPrinter; use this in `collector`
//   and add to ProcessDocBookDirect...
// TODO add static site server/generator.
// TODO consolidate all js, css etc. files in `asset` (singular)
// TODO fix favicon to the default `favicon.ico` and convert the Alter Rebbe picture.
abstract class Site[S <: Site[S]](
  val title: Site.Title.Value,
  val siteUrl: String,
  val favicon: String,
  val pages: Seq[String],
  val licenseName: String,
  val licenseUrl: String,
  val googleAnalyticsId: Option[String],
  val isMathJaxEnabled: Boolean,
  val useMathJax3: Boolean,
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
    texDelimiters = Delimiters("$$"),
    texInlineDelimiters = Delimiters("$")
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

  final def resolve(url: String): Caching.Parser[Option[Store.Path]] = resolve(Files.splitAndDecodeUrl(url))

  def resolve(path: Seq[String]): Caching.Parser[Option[Store.Path]]

  final def a(htmlContent: HtmlContent[S]): html.a = a(htmlContent.path(this))

  def a(path: Store.Path): html.a
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
  val useMathJax3Attribute: Attribute.OrDefault[Boolean] = new Attribute.BooleanAttribute("useMathJax3").orDefault
  val emailAttribute: Attribute.Required[String] = Attribute("email").required
  val githubUsernameAttribute: Attribute.Optional[String] = Attribute("githubUsername").optional
  val twitterUsernameAttribute: Attribute.Optional[String] = Attribute("twitterUsername").optional

  def prettyPrint(roots: List[File], f: Xml.Element => (PrettyPrinter, Option[Doctype])): Unit = {
    val (directories: List[File], files: List[File]) = roots.partition(_.isDirectory)
    val xmlFiles: List[File] = files.filter(isXml) ++ listXmlFiles(List.empty, directories)
    xmlFiles.foreach { file =>
      val xml: Xml.Element = Xml.loadFromUrl(Files.file2url(file))
      val (prettyPrinter, doctype) = f(xml)
      Files.write(
        file = file,
        content = prettyPrinter.renderXml(xml, doctype)
      )
    }
  }

  @tailrec
  def listXmlFiles(result: List[File], directoriesToList: List[File]): List[File] = directoriesToList match {
    case Nil => result
    case current :: tail =>
      val (directories: List[File], files: List[File]) = current.listFiles.toList.partition(_.isDirectory)
      listXmlFiles(result ++ files.filter(isXml), tail ++ directories)
  }

  private def isXml(file: File): Boolean = Files.nameAndExtension(file.getName)._2.contains("xml")
}
