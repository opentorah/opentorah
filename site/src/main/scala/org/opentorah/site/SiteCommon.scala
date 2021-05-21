package org.opentorah.site

import org.opentorah.xml.{Attribute, Element, Parsable, Parser, RawXml, Unparser}

final class SiteCommon(
  val url: Option[String],
  val favicon: Option[String],
  val googleAnalyticsId: Option[String],
  val email: Option[String],
  val title: Option[SiteCommon.Title.Value],
  val license: Option[SiteLicense],
  private val social: Option[SiteSocial],
  val footer: Option[SiteCommon.Footer.Value],
  val pages: Seq[String],
  private val tei: Option[SiteTei],
  private val highlighter: Option[SiteHighlighter],
  private val mathJax: Option[SiteMathJax]
) {
  def getSocial     : SiteSocial      = social     .getOrElse(SiteSocial     .empty)
  def getTei        : SiteTei         = tei        .getOrElse(SiteTei        .empty)
  def getHighlighter: SiteHighlighter = highlighter.getOrElse(SiteHighlighter.empty)
  def getMathJax    : SiteMathJax     = mathJax    .getOrElse(SiteMathJax    .empty)
}

object SiteCommon extends Element[SiteCommon]("common") {
  object Title  extends RawXml("title")
  object Footer extends RawXml("footer")

  private val urlAttribute: Attribute.Optional[String] = Attribute("url").optional
  private val faviconAttribute: Attribute.Optional[String] = Attribute("favicon").optional
  private val googleAnalyticsIdAttribute: Attribute.Optional[String] = Attribute("googleAnalyticsId").optional
  private val emailAttribute: Attribute.Optional[String] = Attribute("email").optional

  override def contentParsable: Parsable[SiteCommon] = new Parsable[SiteCommon] {
    override def parser: Parser[SiteCommon] = for {
      url <- urlAttribute()
      favicon <- faviconAttribute()
      googleAnalyticsId <- googleAnalyticsIdAttribute()
      email <- emailAttribute()
      title <- Title.element.optional()
      license <- SiteLicense.optional()
      social <- SiteSocial.optional()
      footer <- Footer.element.optional()
      pages <- SitePage.seq()
      tei <- SiteTei.optional()
      highlighter <- SiteHighlighter.optional()
      mathJax <- SiteMathJax.optional()
    } yield new SiteCommon(
      url,
      favicon,
      googleAnalyticsId,
      email,
      title,
      license,
      social,
      footer,
      pages,
      tei,
      highlighter,
      mathJax
    )

    override def unparser: Unparser[SiteCommon] = Unparser.concat[SiteCommon](
      urlAttribute(_.url),
      faviconAttribute(_.favicon),
      googleAnalyticsIdAttribute(_.googleAnalyticsId),
      emailAttribute(_.email),
      Title.element.optional(_.title),
      SiteLicense.optional(_.license),
      SiteSocial.optional(_.social),
      Footer.element.optional(_.footer),
      SitePage.seq(_.pages),
      SiteTei.optional(_.tei),
      SiteHighlighter.optional(_.highlighter),
      SiteMathJax.optional(_.mathJax)
    )
  }
}
