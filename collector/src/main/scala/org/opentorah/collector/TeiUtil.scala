package org.opentorah.collector

import org.opentorah.tei.{Availability, CalendarDesc, LangUsage, Language, ProfileDesc, PublicationStmt, Publisher,
  SourceDesc, Tei}
import org.opentorah.xml.PaigesPrettyPrinter

object TeiUtil {

  val teiPrettyPrinter: PaigesPrettyPrinter = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )

  val htmlPrettyPrinter: PaigesPrettyPrinter = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
  )

  val addPublicationStatement: Tei => Tei = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = Some(new PublicationStmt(
          publisher = Some(new Publisher.Value(<ptr target="www.alter-rebbe.org"/>)),
          availability = Some(new Availability(
            status = Some("free"),
            xml = <licence><ab><ref n="license" target="http://creativecommons.org/licenses/by/4.0/">
                  Creative Commons Attribution 4.0 International License</ref></ab></licence>)))))))

  val addSourceDesc: Tei => Tei = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        sourceDesc = Some(new SourceDesc.Value(<p>Facsimile</p>)))))

  val addCalendarDesc: Tei => Tei = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      profileDesc = Some(tei.teiHeader.profileDesc.getOrElse(ProfileDesc()).copy(
        calendarDesc = Some(new CalendarDesc.Value(<calendar xml:id="julian"><p>Julian calendar</p></calendar>))))))

  val addLanguage: Tei => Tei = tei => {
    val textLang: Option[String] = tei.text.lang
    val langUsage: Option[LangUsage] = tei.teiHeader.profileDesc.flatMap(_.langUsage)
    val add: Boolean = langUsage.isEmpty && textLang.isDefined
    if (!add) tei else tei.copy(teiHeader = tei.teiHeader.copy(
      profileDesc = Some(tei.teiHeader.profileDesc.getOrElse(ProfileDesc()).copy(langUsage =
        Some(LangUsage(languages = Seq(Language(
          ident = textLang.get,
          usage = None,
          text = None
        ))))))
    ))
  }

  val addCommon: Tei => Tei =
    addPublicationStatement compose addSourceDesc compose addCalendarDesc compose addLanguage

  val addCommonNoCalendar: Tei => Tei =
    addPublicationStatement compose addSourceDesc compose addLanguage

  val removeLanguage: Tei => Tei = tei => {
    val textLang: Option[String] = tei.text.lang
    val langUsage: Option[LangUsage] = tei.teiHeader.profileDesc.flatMap(_.langUsage)
    val languages: Seq[Language] = langUsage.toSeq.flatMap(_.languages)
    val remove: Boolean = (languages.length == 1) && textLang.contains(languages.head.ident)
    if (!remove) tei else tei.copy(teiHeader = tei.teiHeader.copy(
      profileDesc = Some(tei.teiHeader.profileDesc.get.copy(langUsage = None))
    ))
  }

  def removeCommon(tei: Tei): Tei = {
    removeLanguage(tei)

//    tei.copy(teiHeader = tei.teiHeader.copy(
//      fileDesc = tei.teiHeader.fileDesc.copy(publicationStmt = None, sourceDesc = None),
//      profileDesc = Some(tei.teiHeader.profileDesc.get.copy(calendarDesc = None))
//    ))
  }
}
