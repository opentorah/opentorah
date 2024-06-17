package org.opentorah.site

import org.opentorah.tei.{CalendarDesc, SourceDesc}
import org.opentorah.xml.{Attribute, ElementTo, Parsable, Parser, Unparser}

class SiteTei(
  val facsimilesUrl: Option[String],
  val sourceDesc: Option[SourceDesc.Value],
  val calendarDesc: Option[CalendarDesc.Value]
)

object SiteTei extends ElementTo[SiteTei]("tei"):
  val empty: SiteTei = SiteTei(
    facsimilesUrl = None,
    sourceDesc = None,
    calendarDesc = None
  )

  override def contentParsable: Parsable[SiteTei] = new Parsable[SiteTei]:
    private val facsimilesUrlAttribute: Attribute.Optional[String] = Attribute("facsimilesUrl").optional

    override def parser: Parser[SiteTei] = for
      facsimilesUrl: Option[String] <- facsimilesUrlAttribute()
      sourceDesc: Option[SourceDesc.Value] <- SourceDesc.element.optional()
      calendarDesc: Option[CalendarDesc.Value] <- CalendarDesc.element.optional()
    yield SiteTei(
      facsimilesUrl,
      sourceDesc,
      calendarDesc
    )

    override def unparser: Unparser[SiteTei] = Unparser.concat[SiteTei](
      facsimilesUrlAttribute(_.facsimilesUrl),
      SourceDesc.element.optional(_.sourceDesc),
      CalendarDesc.element.optional(_.calendarDesc),
    )
