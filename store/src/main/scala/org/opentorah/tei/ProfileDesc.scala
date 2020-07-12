package org.opentorah.tei

import org.opentorah.xml.{Attribute, Choice, Element, Parser}
import scala.xml.Elem

final case class ProfileDesc(
  documentAbstract: Option[Abstract.Value],
  creation: Option[Creation],
  langUsage: Option[LangUsage],
  textClass: Option[TextClass.Value],
  correspDesc: Option[CorrespDesc.Value],
  calendarDesc: Option[CalendarDesc.Value],
  handNotes: Option[HandNotes],
  listTranspose: Option[ListTranspose.Value]
)

object ProfileDesc extends Element.WithToXml[ProfileDesc]("profileDesc") {

  override protected def parser: Parser[ProfileDesc] = for {
    values <- Choice(Seq(
      Abstract.parsable,
      Creation,
      LangUsage,
      TextClass.parsable,
      CorrespDesc.parsable,
      CalendarDesc.parsable,
      HandNotes,
      ListTranspose.parsable
    ))
    documentAbstract <- values.optional(Abstract.parsable)
    creation <- values.optional(Creation)
    langUsage <- values.optional(LangUsage)
    textClass <- values.optional(TextClass.parsable)
    correspDesc <- values.optional(CorrespDesc.parsable)
    calendarDesc <- values.optional(CalendarDesc.parsable)
    handNotes <- values.optional(HandNotes)
    listTranspose <- values.optional(ListTranspose.parsable)
  } yield new ProfileDesc(
    documentAbstract,
    creation,
    langUsage,
    textClass,
    correspDesc,
    calendarDesc,
    handNotes,
    listTranspose
  )

  override protected def attributes(value: ProfileDesc): Seq[Attribute.Value[_]] = Seq.empty

  override protected def content(value: ProfileDesc): Seq[Elem] =
    Abstract.parsable.toXml(value.documentAbstract) ++
    Creation.toXml(value.creation) ++
    LangUsage.toXml(value.langUsage) ++
    TextClass.parsable.toXml(value.textClass) ++
    CorrespDesc.parsable.toXml(value.correspDesc) ++
    CalendarDesc.parsable.toXml(value.calendarDesc) ++
    HandNotes.toXml(value.handNotes) ++
    ListTranspose.parsable.toXml(value.listTranspose)

  def apply(): ProfileDesc = new ProfileDesc(
    documentAbstract = None,
    creation = None,
    langUsage = None,
    textClass = None,
    correspDesc = None,
    calendarDesc = None,
    handNotes = None,
    listTranspose = None
  )
}
