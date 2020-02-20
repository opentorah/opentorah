package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.{ContentType, Parser, Xml}

final case class ProfileDesc(
  langUsage: Option[LangUsage],
  calendarDesc: Option[CalendarDesc],
  correspDesc: Option[CorrespDesc],
  creation: Option[Creation],
  documentAbstract: Option[Abstract]
)

object ProfileDesc {
  val elementName: String = "profileDesc"

  val parser: Parser[ProfileDesc] = for {
    langUsage <- LangUsage.optionalParser
    calendarDesc <- CalendarDesc.optionalParser
    creation <- Creation.optionalParser
    documentAbstract <- Abstract.optionalParser
    correspDesc <- CorrespDesc.optionalParser
  } yield ProfileDesc(
    langUsage,
    calendarDesc,
    correspDesc,
    creation,
    documentAbstract
  )

  val optionalParser: Parser[Option[ProfileDesc]] =
    Xml.element.optional(elementName, ContentType.Elements, parser)
}
