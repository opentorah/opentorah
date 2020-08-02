package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Namespace, Parser, PrettyPrinter, Xml}
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

object Tei extends Element.WithToXml[Tei]("TEI") {

  type Transformer = Tei => Tei

  override protected val parser: Parser[Tei] = for {
    teiHeader <- TeiHeader.required
    text <- Text.required
  } yield new Tei(
    teiHeader,
    text
  )

  override protected val antiparser: Antiparser[Tei] = Antiparser(
    attributes = _ => Seq(Namespace.Tei.xmlnsAttribute),
    content = value => Seq(
      Xml.removeNamespace(TeiHeader.toXml(value.teiHeader)),
      Xml.removeNamespace(Text.toXml(value.text))
    )
  )

  def apply(body: Seq[Node]): Tei = new Tei(
    teiHeader = TeiHeader(),
    text = new Text(
      lang = None,
      new Body.Value(body)
    )
  )

  val prettyPrinter: PrettyPrinter = new PrettyPrinter(
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )
}
