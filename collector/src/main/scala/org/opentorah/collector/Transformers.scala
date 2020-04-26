package org.opentorah.collector

import org.opentorah.entity.EntityType
import org.opentorah.tei.{Availability, CalendarDesc, LangUsage, Language, ProfileDesc, PublicationStmt, Publisher,
  SourceDesc, Tei}
import org.opentorah.util.{Files, Xml}
import org.opentorah.xml.PaigesPrettyPrinter
import scala.xml.{Attribute, Elem, Node}

object Transformers {

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

  def multi(nodes: Seq[Node]): Seq[Node] = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: ns if n.isInstanceOf[Elem] => Seq(n, Xml.textNode(", ")) ++ multi(ns)
    case n :: ns => Seq(n) ++ multi(ns)
    case n => n
  }

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

  def refTransformer(site: Site): Xml.Transformer = elem => if (elem.label != "ref") elem else {
    if (elem.child.forall(Xml.isWhitespace)) println(s"No reference text: $elem")
    elem.attribute("target").map(_.text).fold(throw new IllegalArgumentException(s"empty target: $elem")) { target =>
      if (!target.startsWith("/")) elem else {
        val (url, part) = Files.urlAndPart(target)
        site.resolve(url).fold {
          println(s"did not resolve: $target")
          elem
        } { resolved =>
          val roleShouldBe: Option[String] = resolved match {
            case teiWrapperFile: TeiWrapperFile => Some(teiWrapperFile.viewer.name)
            case _ => None
          }
          val role: Option[String] = elem.attribute("role").map(_.text)
          if (role.isDefined) println(s"role discrepancy")
          if ((role == roleShouldBe) || roleShouldBe.isEmpty || role.isDefined) elem
          else {
            val target: String = Files.mkUrl(Files.addPart(resolved.url, part))
            val rendition: Option[String] = elem.attribute("rendition").map(_.text)
            elem.copy(attributes =
              Attribute("role", Xml.textNode(roleShouldBe.get),
              Attribute("target", Xml.textNode(target),
              rendition.fold[scala.xml.MetaData](scala.xml.Null)(rendition =>
                Attribute("rendition", Xml.textNode(rendition), scala.xml.Null)))))
          }
        }
      }
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

  def nameTransformer(site: Site): Xml.Transformer = elem => if (EntityType.forName(elem.label).isEmpty) elem else {
    elem.attribute("ref").map(_.text).fold(elem){ ref =>
      site.findByRef(ref).fold {
        println(s"did not find reference: $ref")
        elem
      }{ entity =>
        val target: String = Files.mkUrl(EntityObject.teiWrapperUrl(entity))
        elem.copy(attributes =
          Attribute("role", Xml.textNode(Viewer.Names.name),
          Attribute("target", Xml.textNode(target), scala.xml.Null)))
      }
    }
  }
}
