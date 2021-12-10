package org.opentorah.site

import org.opentorah.docbook.DocBookConfiguration
import org.opentorah.html.SiteHtml
import org.opentorah.metadata.Names
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class SiteCommon(
  val names: Names,
  val html: Option[SiteHtml],
  val license: Option[SiteLicense],
  val pages: Seq[String],
  val docbook: Option[DocBookConfiguration],
  private val tei: Option[SiteTei],
  val isStatic: Option[Boolean]
):
  def getHtml: SiteHtml = html.getOrElse(SiteHtml.empty)
  def getTei : SiteTei  = tei .getOrElse(SiteTei .empty)

object SiteCommon extends Element[SiteCommon]("common"):

  override def contentParsable: Parsable[SiteCommon] = new Parsable[SiteCommon]:
    private val isStaticAttribute: Attribute.Optional[Boolean] = new Attribute.BooleanAttribute("isStatic").optional
  
    override def parser: Parser[SiteCommon] = for
      names: Names <- Names.withDefaultNameParsable()
      html: Option[SiteHtml] <- SiteHtml.optional()
      license: Option[SiteLicense] <- SiteLicense.optional()
      pages: Seq[String] <- SitePage.seq()
      tei: Option[SiteTei] <- SiteTei.optional()
      docbook: Option[DocBookConfiguration] <- DocBookConfiguration.followRedirects.optional()
      isStatic: Option[Boolean] <- isStaticAttribute()
    yield SiteCommon(
      names,
      html,
      license,
      pages,
      docbook,
      tei,
      isStatic
    )

    override def unparser: Unparser[SiteCommon] = Unparser.concat[SiteCommon](
      Names.withDefaultNameParsable(_.names),
      SiteHtml.optional(_.html),
      SiteLicense.optional(_.license),
      SitePage.seq(_.pages),
      DocBookConfiguration.optional(_.docbook),
      SiteTei.optional(_.tei),
      isStaticAttribute(_.isStatic)
    )
