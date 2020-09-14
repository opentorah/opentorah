package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Dialect, Element, Namespace, Parser, PrettyPrinter}
import scala.xml.Node

final case class Tei(
  teiHeader: TeiHeader,
  text: Text
) {
  def titleStmt: TitleStmt = teiHeader.fileDesc.titleStmt
  val correspDesc: Option[CorrespDesc.Value] = teiHeader.profileDesc.flatMap(_.correspDesc)
  def getAbstract: Option[Seq[Node]] = teiHeader.profileDesc.flatMap(_.documentAbstract.map(_.xml))
  def creationDate: Option[Date] = teiHeader.profileDesc.flatMap(_.creation.map(_.date))
  def languages: Seq[Language] = teiHeader.profileDesc.flatMap(_.langUsage).toSeq.flatMap(_.languages)
  val body: Body.Value = text.body
  val pbs: Seq[Pb] = body.xml.flatMap(Pb.descendants)

  def addressee: Option[EntityReference] =
    EntityReference.from(correspDesc.map(_.xml).getOrElse(Seq.empty))
      .find(name => (name.entityType == EntityType.Person) && name.role.contains("addressee"))

  /////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>""" + "\n" +
}

object Tei extends Element.WithToXml[Tei]("TEI") with Dialect {

  override val namespace: Namespace = Namespace(uri = "http://www.tei-c.org/ns/1.0", prefix="tei")

  override val mimeType: String = "application/tei+xml"

  override val prettyPrinter: PrettyPrinter = new PrettyPrinter(
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )

  type Transformer = Tei => Tei

  override protected lazy val parser: Parser[Tei] = for {
    teiHeader <- TeiHeader.required
    text <- Text.required
  } yield new Tei(
    teiHeader,
    text
  )

  override protected lazy val antiparser: Antiparser[Tei] = concat(
    TeiHeader.toXml.compose(_.teiHeader),
    Text.toXml.compose(_.text)
  )

  def concat[A](antiparsers: Antiparser[A]*): Antiparser[A] =
    Antiparser.concat(Some(Tei.namespace), antiparsers)

  def apply(body: Seq[Node]): Tei = new Tei(
    teiHeader = TeiHeader(),
    text = new Text(
      lang = None,
      new Body.Value(body)
    )
  )

  def addPublicationStatement(
    publisher: Publisher.Value,
    status: String,
    licenseName: String,
    licenseUrl: String
  ): Tei.Transformer = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = Some(new PublicationStmt(
          publisher = Some(publisher),
          availability = Some(new Availability(
            status = Some(status),
            xml = <licence><ab><ref n="license" target={licenseUrl}>{licenseName}</ref></ab></licence>)))))))

  def addSourceDesc(value: SourceDesc.Value): Tei.Transformer = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        sourceDesc = Some(value))))

  def addCalendarDesc(value: CalendarDesc.Value): Tei.Transformer = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      profileDesc = Some(tei.teiHeader.profileDesc.getOrElse(ProfileDesc()).copy(
        calendarDesc = Some(value)))))

  val addLanguage: Transformer = tei => {
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
}
