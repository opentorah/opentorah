package org.opentorah.archive.collector

import org.opentorah.tei.{Availability, CalendarDesc, ProfileDesc, PublicationStmt, Publisher, SourceDesc, Tei}
import org.opentorah.xml.PaigesPrettyPrinter

object TeiUtil {

  val teiPrettyPrinter: PaigesPrettyPrinter = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline", "item"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )

  val htmlPrettyPrinter: PaigesPrettyPrinter = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
  )

  // TODO generate langUsage from xml:lang in the body.

  val addPublicationStatement: Tei => Tei = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = new PublicationStmt(
          publisher = Some(new Publisher.Value(<ptr target="www.alter-rebbe.org"/>)),
          availability = Some(new Availability(
            status = Some("free"),
            xml = <licence><ab><ref n="license" target="http://creativecommons.org/licenses/by/4.0/">
                  Creative Commons Attribution 4.0 International License</ref></ab></licence>))))))

  val addSourceDesc: Tei => Tei = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        sourceDesc = new SourceDesc.Value(<p>Facsimile</p>))))

  val addCalendarDesc: Tei => Tei = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      profileDesc = Some(tei.teiHeader.profileDesc.getOrElse(ProfileDesc()).copy(
        calendarDesc = Some(new CalendarDesc.Value(<calendar xml:id="julian"><p>Julian calendar</p></calendar>))))))

  val addCommon: Tei => Tei =
    addPublicationStatement compose addSourceDesc compose addCalendarDesc

  val addCommonNoCalendar: Tei => Tei =
    addPublicationStatement compose addSourceDesc

  def removeCommon(tei: Tei): Tei = {
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = new PublicationStmt(
          publisher = None,
          availability = None
        ),
        sourceDesc = new SourceDesc.Value(Seq.empty)
      ),
      profileDesc = Some(tei.teiHeader.profileDesc.get.copy(
        calendarDesc = None
      ))
    ))
  }
}
