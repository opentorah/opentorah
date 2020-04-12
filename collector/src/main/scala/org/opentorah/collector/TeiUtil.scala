package org.opentorah.collector

import org.opentorah.tei.{Availability, CalendarDesc, LangUsage, Language, ProfileDesc, PublicationStmt, Publisher,
  SourceDesc, Tei}
import org.opentorah.xml.PaigesPrettyPrinter
import scala.xml.{Elem, Node}

object TeiUtil {

  val xmlHeader: String = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n"

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

//  val removeLanguage: Tei => Tei = tei => {
//    val textLang: Option[String] = tei.text.lang
//    val langUsage: Option[LangUsage] = tei.teiHeader.profileDesc.flatMap(_.langUsage)
//    val languages: Seq[Language] = langUsage.toSeq.flatMap(_.languages)
//    val remove: Boolean = (languages.length == 1) && textLang.contains(languages.head.ident)
//    if (!remove) tei else tei.copy(teiHeader = tei.teiHeader.copy(
//      profileDesc = Some(tei.teiHeader.profileDesc.get.copy(langUsage = None))
//    ))
//  }

  def removeCommon(tei: Tei): Tei = {
//    removeLanguage(tei)

//    tei.copy(teiHeader = tei.teiHeader.copy(
//      fileDesc = tei.teiHeader.fileDesc.copy(publicationStmt = None, sourceDesc = None),
//      profileDesc = Some(tei.teiHeader.profileDesc.get.copy(calendarDesc = None))
//    ))
    tei
  }

  def refRoleRewriter(site: Site): Elem => Elem = elem =>
    if (elem.label != "ref") elem else {
      elem.attribute("target").map(_.text).fold(throw new IllegalArgumentException(s"empty target: $elem")) { target =>
        if (!target.startsWith("/")) elem else {
          val roleShouldBe: Option[String] = site.resolve(target).map(_.siteObject.viewer)
          val role: Option[String] = elem.attribute("role").map(_.text)
          if (roleShouldBe.isEmpty) println(s"did not resolve: $target")
          if (roleShouldBe.isDefined && role.isDefined && (role != roleShouldBe)) println(s"role discrepancy")
          if ((role == roleShouldBe) || roleShouldBe.isEmpty || role.isDefined) elem
          else elem % scala.xml.Attribute(None, "role", textNode(roleShouldBe.get), scala.xml.Null)
        }
      }
    }

  def sourcePbsRewriter(collectionName: String): Elem => Elem = elem =>
    if (elem.label != "pb") elem else {
      val n: String = elem.attribute("n").map(_.text).get
      val facs: Option[String] = elem.attribute("facs").map(_.text)
      val shouldBe = "http://facsimiles.alter-rebbe.org/facsimiles/" + collectionName + "/" + n + ".jpg"
      if (facs.isDefined && !facs.contains(shouldBe)) println(s"shouldBe $shouldBe but got $facs")
      elem
    }

  def multi(nodes: Seq[Node]): Seq[Node] = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: ns if n.isInstanceOf[Elem] => Seq(n, textNode(", ")) ++ multi(ns)
    case n :: ns => Seq(n) ++ multi(ns)
    case n => n
  }

  def textNode(text: String): Node = new scala.xml.Text(text)

  def spacedText(nodes: Seq[Node]): String = nodes.map(spacedText).mkString("")
  def spacedText(node: Node): String = {
    val result = node match {
      case elem: Elem => (elem.child map (_.text)).mkString(" ")
      case node: Node => node.text
    }
    result
      .replace('\n', ' ')
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
  }
}
