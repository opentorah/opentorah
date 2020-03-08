package org.opentorah.tei

import org.opentorah.xml.{Descriptor, Repeatable}
import scala.xml.Elem

// TODO also: abstract handNotes listTranspose particDesc settingDesc textDesc
final case class ProfileDesc(
  documentAbstract: Option[Abstract],
  creation: Option[Creation],
  langUsage: Option[LangUsage],
  textClass: Option[TextClass],
  correspDesc: Option[CorrespDesc],
  calendarDesc: Option[CalendarDesc]
)

object ProfileDesc extends Descriptor[ProfileDesc](
  elementName = "profileDesc",
  contentParser = for {
      values <- Repeatable.choice(Seq(
        LangUsage,
        CalendarDesc,
        Creation,
        Abstract,
        TextClass,
        CorrespDesc
      )).toMap
    } yield new ProfileDesc(
      // TODO yuck!!!
      values.get(Abstract).map(_.asInstanceOf[Abstract]),
      values.get(Creation).map(_.asInstanceOf[Creation]),
      values.get(LangUsage).map(_.asInstanceOf[LangUsage]),
      values.get(TextClass).map(_.asInstanceOf[TextClass]),
      values.get(CorrespDesc).map(_.asInstanceOf[CorrespDesc]),
      values.get(CalendarDesc).map(_.asInstanceOf[CalendarDesc])
    )
) {
  override def toXml(value: ProfileDesc): Elem =
    <profileDesc>
      {Abstract.toXml(value.documentAbstract)}
      {Creation.toXml(value.creation)}
      {LangUsage.toXml(value.langUsage)}
      {TextClass.toXml(value.textClass)}
      {CorrespDesc.toXml(value.correspDesc)}
      {CalendarDesc.toXml(value.calendarDesc)}
    </profileDesc>
}
