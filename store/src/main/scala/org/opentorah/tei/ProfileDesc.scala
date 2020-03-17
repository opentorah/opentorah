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
        LangUsage,
        CalendarDesc.parsable,
        Creation,
        Abstract.parsable,
        TextClass.parsable,
        CorrespDesc.parsable
      ))
    } yield new ProfileDesc(
      values.one(Abstract.parsable),
      values.one(Creation),
      values.one(LangUsage),
      values.one(TextClass.parsable),
      values.one(CorrespDesc.parsable),
      values.one(CalendarDesc.parsable)
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
}
