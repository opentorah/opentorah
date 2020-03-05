package org.digitaljudaica.tei

import org.digitaljudaica.xml.Descriptor

// TODO also: abstract handNotes listTranspose particDesc settingDesc textDesc
// TODO parse multiple instances in ANY ORDER
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
    // TODO the order is wrong; once I can write TEI XML decently, re-order in the existing documents:
    langUsage <- LangUsage.optional
    calendarDesc <- CalendarDesc.optional
    creation <- Creation.optional
    documentAbstract <- Abstract.optional
    textClass <- TextClass.optional
    correspDesc <- CorrespDesc.optional
  } yield new ProfileDesc(
    documentAbstract,
    creation,
    langUsage,
    textClass,
    correspDesc,
    calendarDesc
  ),
  toXml = (value: ProfileDesc) =>
    <profileDesc>
      {Abstract.toXml(value.documentAbstract)}
      {Creation.toXml(value.creation)}
      {LangUsage.toXml(value.langUsage)}
      {TextClass.toXml(value.textClass)}
      {CorrespDesc.toXml(value.correspDesc)}
      {CalendarDesc.toXml(value.calendarDesc)}
    </profileDesc>
)
