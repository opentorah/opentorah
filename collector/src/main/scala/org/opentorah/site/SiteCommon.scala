package org.opentorah.site

import org.opentorah.metadata.Names
import org.opentorah.xml.{Attribute, ElementTo, Parsable, Parser, Unparser}

final class SiteCommon(
  val names: Names,
  val html: Option[SiteHtml],
  val license: Option[SiteLicense],
  val pages: Seq[String],
  private val tei: Option[SiteTei],
  val isStatic: Option[Boolean]
):
  def getHtml: SiteHtml = html.getOrElse(SiteHtml.empty)
  def getTei : SiteTei  = tei .getOrElse(SiteTei .empty)

object SiteCommon extends ElementTo[SiteCommon]("common"):

  override def contentParsable: Parsable[SiteCommon] = new Parsable[SiteCommon]:
    private val isStaticAttribute: Attribute.Optional[Boolean] = new Attribute.BooleanAttribute("isStatic").optional
  
    override def parser: Parser[SiteCommon] = for
      names: Names <- Names.withDefaultNameParsable()
      html: Option[SiteHtml] <- SiteHtml.optional()
      license: Option[SiteLicense] <- SiteLicense.optional()
      pages: Seq[String] <- SitePage.seq()
      tei: Option[SiteTei] <- SiteTei.optional()
      isStatic: Option[Boolean] <- isStaticAttribute()
    yield SiteCommon(
      names,
      html,
      license,
      pages,
      tei,
      isStatic
    )

    override def unparser: Unparser[SiteCommon] = Unparser.concat[SiteCommon](
      Names.withDefaultNameParsable(_.names),
      SiteHtml.optional(_.html),
      SiteLicense.optional(_.license),
      SitePage.seq(_.pages),
      SiteTei.optional(_.tei),
      isStaticAttribute(_.isStatic)
    )
