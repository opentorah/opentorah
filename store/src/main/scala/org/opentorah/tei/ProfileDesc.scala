package org.opentorah.tei

import org.opentorah.xml.{Choice, Element, ToXml}
import scala.xml.Elem

// TODO also: abstract handNotes listTranspose particDesc settingDesc textDesc
final case class ProfileDesc(
  documentAbstract: Option[Abstract.Value],
  creation: Option[Creation],
  langUsage: Option[LangUsage],
  textClass: Option[TextClass.Value],
  correspDesc: Option[CorrespDesc.Value],
  calendarDesc: Option[CalendarDesc.Value]
)

object ProfileDesc extends Element[ProfileDesc](
  elementName = "profileDesc",
  parser = for {
    values <- Choice(Seq(
      Abstract.parsable,
      Creation,
      LangUsage,
      TextClass.parsable,
      CorrespDesc.parsable,
      CalendarDesc.parsable
    ))
    documentAbstract <- values.optional(Abstract.parsable)
    creation <- values.optional(Creation)
    langUsage <- values.optional(LangUsage)
    textClass <- values.optional(TextClass.parsable)
    correspDesc <- values.optional(CorrespDesc.parsable)
    calendarDesc <- values.optional(CalendarDesc.parsable)
  } yield new ProfileDesc(
    documentAbstract,
    creation,
    langUsage,
    textClass,
    correspDesc,
    calendarDesc
  )
) with ToXml[ProfileDesc] {

  override def toXml(value: ProfileDesc): Elem =
    <profileDesc>
      {Abstract.parsable.toXml(value.documentAbstract)}
      {Creation.toXml(value.creation)}
      {LangUsage.toXml(value.langUsage)}
      {TextClass.parsable.toXml(value.textClass)}
      {CorrespDesc.parsable.toXml(value.correspDesc)}
      {CalendarDesc.parsable.toXml(value.calendarDesc)}
    </profileDesc>

  def apply(): ProfileDesc = new ProfileDesc(
    documentAbstract = None,
    creation = None,
    langUsage = None,
    textClass = None,
    correspDesc = None,
    calendarDesc = None
  )
}
