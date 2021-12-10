package org.opentorah.html

import org.opentorah.math.MathConfiguration
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, RawXml, Unparser}

final class SiteHtml(
  val url: Option[String],
  val favicon: Option[String],
  val googleAnalyticsId: Option[String],
  val email: Option[String],
  val title: Option[SiteHtml.Title.Value],
  private val social: Option[SiteSocial],
  val footer: Option[SiteHtml.Footer.Value],
  private val highlighter: Option[SiteHighlighter],
  val math: Option[MathConfiguration]
):
  def getSocial     : SiteSocial        = social     .getOrElse(SiteSocial       .empty  )
  def getHighlighter: SiteHighlighter   = highlighter.getOrElse(SiteHighlighter  .empty  )

object SiteHtml extends Element[SiteHtml]("html"):

  val empty: SiteHtml = SiteHtml(
    url = None,
    favicon = None,
    googleAnalyticsId = None,
    email = None,
    title = None,
    social = None,
    footer = None,
    highlighter = None,
    math = None
  )

  object Title  extends RawXml("title")
  object Footer extends RawXml("footer")

  override def contentParsable: Parsable[SiteHtml] = new Parsable[SiteHtml]:
    private val urlAttribute: Attribute.Optional[String] = Attribute("url").optional
    private val faviconAttribute: Attribute.Optional[String] = Attribute("favicon").optional
    private val googleAnalyticsIdAttribute: Attribute.Optional[String] = Attribute("googleAnalyticsId").optional
    private val emailAttribute: Attribute.Optional[String] = Attribute("email").optional
    private val isStaticAttribute: Attribute.Optional[Boolean] = new Attribute.BooleanAttribute("isStatic").optional

    override def parser: Parser[SiteHtml] = for
      url: Option[String] <- urlAttribute()
      favicon: Option[String] <- faviconAttribute()
      googleAnalyticsId: Option[String] <- googleAnalyticsIdAttribute()
      email: Option[String] <- emailAttribute()
      title: Option[Title.Value] <- Title.element.optional()
      social: Option[SiteSocial] <- SiteSocial.optional()
      footer: Option[Footer.Value] <- Footer.element.optional()
      highlighter: Option[SiteHighlighter] <- SiteHighlighter.optional()
      math: Option[MathConfiguration] <- MathConfiguration.optional()
    yield SiteHtml(
      url,
      favicon,
      googleAnalyticsId,
      email,
      title,
      social,
      footer,
      highlighter,
      math
    )

    override def unparser: Unparser[SiteHtml] = Unparser.concat[SiteHtml](
      urlAttribute(_.url),
      faviconAttribute(_.favicon),
      googleAnalyticsIdAttribute(_.googleAnalyticsId),
      emailAttribute(_.email),
      Title.element.optional(_.title),
      SiteSocial.optional(_.social),
      Footer.element.optional(_.footer),
      SiteHighlighter.optional(_.highlighter),
      MathConfiguration.optional(_.math)
    )
