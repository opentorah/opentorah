package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.Descriptor

final case class ProfileDesc(
  langUsage: Option[LangUsage],
  calendarDesc: Option[CalendarDesc],
  correspDesc: Option[CorrespDesc],
  creation: Option[Creation],
  documentAbstract: Option[Abstract]
)

object ProfileDesc extends Descriptor[ProfileDesc](
  elementName = "profileDesc",
  contentParser = for {
    langUsage <- LangUsage.optional
    calendarDesc <- CalendarDesc.optional
    creation <- Creation.optional
    documentAbstract <- Abstract.optional
    correspDesc <- CorrespDesc.optional
  } yield new ProfileDesc(
    langUsage,
    calendarDesc,
    correspDesc,
    creation,
    documentAbstract
  )
)
