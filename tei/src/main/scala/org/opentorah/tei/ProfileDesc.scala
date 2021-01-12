package org.opentorah.tei

import org.opentorah.xml.{Unparser, Element, Elements, Parsable, Parser}

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

object ProfileDesc extends Element[ProfileDesc]("profileDesc") {

  def empty: ProfileDesc = new ProfileDesc(
    documentAbstract = None,
    creation = None,
    langUsage = None,
    textClass = None,
    correspDesc = None,
    calendarDesc = None,
    handNotes = None,
    listTranspose = None
  )

  override def contentParsable: Parsable[ProfileDesc] = new Parsable[ProfileDesc] {
    override val parser: Parser[ProfileDesc] = for {
      values <- Elements.choices(Seq(
        Abstract.element,
        Creation,
        LangUsage,
        TextClass.element,
        CorrespDesc.element,
        CalendarDesc.element,
        HandNotes,
        ListTranspose.element
      ))
      documentAbstract <- values.optional(Abstract.element)
      creation <- values.optional(Creation)
      langUsage <- values.optional(LangUsage)
      textClass <- values.optional(TextClass.element)
      correspDesc <- values.optional(CorrespDesc.element)
      calendarDesc <- values.optional(CalendarDesc.element)
      handNotes <- values.optional(HandNotes)
      listTranspose <- values.optional(ListTranspose.element)
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

    override val unparser: Unparser[ProfileDesc] = Tei.concat(
      Abstract.element.optional(_.documentAbstract),
      Creation.optional(_.creation),
      LangUsage.optional(_.langUsage),
      TextClass.element.optional(_.textClass),
      CorrespDesc.element.optional(_.correspDesc),
      CalendarDesc.element.optional(_.calendarDesc),
      HandNotes.optional(_.handNotes),
      ListTranspose.element.optional(_.listTranspose)
    )
  }
}
