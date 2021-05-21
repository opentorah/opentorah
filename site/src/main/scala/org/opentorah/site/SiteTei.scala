package org.opentorah.site

import org.opentorah.tei.{CalendarDesc, SourceDesc}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

class SiteTei(
  val facsimilesUrl: Option[String],
  val sourceDesc: Option[SourceDesc.Value],
  val calendarDesc: Option[CalendarDesc.Value]
)

object SiteTei extends Element[SiteTei]("tei") {
  val empty: SiteTei = new SiteTei(
    facsimilesUrl = None,
    sourceDesc = None,
    calendarDesc = None
  )

  private val facsimilesUrlAttribute: Attribute.Optional[String] = Attribute("facsimilesUrl").optional

  override def contentParsable: Parsable[SiteTei] = new Parsable[SiteTei] {
    override def parser: Parser[SiteTei] = for {
      facsimilesUrl <- facsimilesUrlAttribute()
      sourceDesc <- SourceDesc.element.optional()
      calendarDesc <- CalendarDesc.element.optional()
    } yield new SiteTei(
      facsimilesUrl,
      sourceDesc,
      calendarDesc
    )

    override def unparser: Unparser[SiteTei] = Unparser.concat[SiteTei](
      facsimilesUrlAttribute(_.facsimilesUrl),
      SourceDesc.element.optional(_.sourceDesc),
      CalendarDesc.element.optional(_.calendarDesc),
    )
  }
}
