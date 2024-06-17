package org.opentorah.tei

import org.opentorah.xml.{ElementTo, ElementsTo, Parsable, Parser, Unparser}

final class ProfileDesc(
  val documentAbstract: Option[Abstract.Value],
  val creation: Option[Creation],
  val langUsage: Option[LangUsage],
  val textClass: Option[TextClass.Value],
  val correspDesc: Option[CorrespDesc.Value],
  val calendarDesc: Option[CalendarDesc.Value],
  val handNotes: Option[HandNotes],
  val listTranspose: Option[ListTranspose.Value]
):
  def copy(
    documentAbstract: Option[Abstract.Value] = documentAbstract,
    creation: Option[Creation] = creation,
    langUsage: Option[LangUsage] = langUsage,
    textClass: Option[TextClass.Value] = textClass,
    correspDesc: Option[CorrespDesc.Value] = correspDesc,
    calendarDesc: Option[CalendarDesc.Value] = calendarDesc,
    handNotes: Option[HandNotes] = handNotes,
    listTranspose: Option[ListTranspose.Value] = listTranspose
  ): ProfileDesc = ProfileDesc(
    documentAbstract,
    creation,
    langUsage,
    textClass,
    correspDesc,
    calendarDesc,
    handNotes,
    listTranspose
  )

object ProfileDesc extends ElementTo[ProfileDesc]("profileDesc"):

  def empty: ProfileDesc = ProfileDesc(
    documentAbstract = None,
    creation = None,
    langUsage = None,
    textClass = None,
    correspDesc = None,
    calendarDesc = None,
    handNotes = None,
    listTranspose = None
  )

  override def contentParsable: Parsable[ProfileDesc] = new Parsable[ProfileDesc]:
    override val parser: Parser[ProfileDesc] = for
      choices: ElementsTo.Choices <- ElementsTo.choices(
        Abstract.element,
        Creation,
        LangUsage,
        TextClass.element,
        CorrespDesc.element,
        CalendarDesc.element,
        HandNotes,
        ListTranspose.element
      )
      documentAbstract: Option[Abstract.Value] <- choices(Abstract.element)
      creation: Option[Creation] <- choices(Creation)
      langUsage: Option[LangUsage] <- choices(LangUsage)
      textClass: Option[TextClass.Value] <- choices(TextClass.element)
      correspDesc: Option[CorrespDesc.Value] <- choices(CorrespDesc.element)
      calendarDesc: Option[CalendarDesc.Value] <- choices(CalendarDesc.element)
      handNotes: Option[HandNotes] <- choices(HandNotes)
      listTranspose: Option[ListTranspose.Value] <- choices(ListTranspose.element)
    yield ProfileDesc(
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
