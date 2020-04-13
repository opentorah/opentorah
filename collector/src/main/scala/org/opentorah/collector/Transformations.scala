package org.opentorah.collector

import org.opentorah.tei.{Availability, CalendarDesc, LangUsage, Language, ProfileDesc, PublicationStmt, Publisher,
  SourceDesc, Tei}
import org.opentorah.util.{Files, Xml}
import org.opentorah.xml.PaigesPrettyPrinter

object Transformations {

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

  val addPublicationStatement: Tei.Transformer = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = Some(new PublicationStmt(
          publisher = Some(new Publisher.Value(<ptr target="www.alter-rebbe.org"/>)),
          availability = Some(new Availability(
            status = Some("free"),
            xml = <licence><ab><ref n="license" target="http://creativecommons.org/licenses/by/4.0/">
                  Creative Commons Attribution 4.0 International License</ref></ab></licence>)))))))

  val addSourceDesc: Tei.Transformer = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        sourceDesc = Some(new SourceDesc.Value(<p>Facsimile</p>)))))

  val addCalendarDesc: Tei.Transformer = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      profileDesc = Some(tei.teiHeader.profileDesc.getOrElse(ProfileDesc()).copy(
        calendarDesc = Some(new CalendarDesc.Value(<calendar xml:id="julian"><p>Julian calendar</p></calendar>))))))

  val addLanguage: Tei.Transformer = tei => {
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

  val addCommon: Tei.Transformer =
    addPublicationStatement compose addSourceDesc compose addCalendarDesc compose addLanguage

  val addCommonNoCalendar: Tei.Transformer =
    addPublicationStatement compose addSourceDesc compose addLanguage

  def refTransformer(site: Site): Xml.Transformer = elem => if (elem.label != "ref") elem else
    elem.attribute("target").map(_.text).fold(throw new IllegalArgumentException(s"empty target: $elem")) { target =>
      if (!target.startsWith("/")) elem else {
        val roleShouldBe: Option[String] = site.resolve(target).flatMap {
          case teiWrapperFile: TeiWrapperFile => Some(teiWrapperFile.viewer.name)
          case _ => None
        }
        val role: Option[String] = elem.attribute("role").map(_.text)
        if (roleShouldBe.isEmpty) println(s"did not resolve: $target")
        if (roleShouldBe.isDefined && role.isDefined && (role != roleShouldBe)) println(s"role discrepancy")
        if ((role == roleShouldBe) || roleShouldBe.isEmpty || role.isDefined) elem
        else elem % scala.xml.Attribute(None, "role", Xml.textNode(roleShouldBe.get), scala.xml.Null)
      }
    }


  def pbTransformer(facsUrl: Seq[String]): Xml.Transformer = elem => if (elem.label != "pb") elem else {
    val pageId: String = Page.pageId(elem.attribute("n").get.text)
    <pb
      xml:id={pageId}
      rendition={Page.pageRendition(elem.attribute("missing").map(_.text).contains("true"))}
      role={Viewer.Facsimile.name}
      target={Files.mkUrl(Files.addPart(facsUrl, pageId))}
    />
  }
}
