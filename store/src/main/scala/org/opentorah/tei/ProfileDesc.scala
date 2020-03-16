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
      )).toMap
    } yield new ProfileDesc(
      // TODO yuck!!!
      values.get(Abstract.parsable).map(_.asInstanceOf[Abstract.Value]),
      values.get(Creation).map(_.asInstanceOf[Creation]),
      values.get(LangUsage).map(_.asInstanceOf[LangUsage]),
      values.get(TextClass.parsable).map(_.asInstanceOf[TextClass.Value]),
      values.get(CorrespDesc.parsable).map(_.asInstanceOf[CorrespDesc.Value]),
      values.get(CalendarDesc.parsable).map(_.asInstanceOf[CalendarDesc.Value])
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
