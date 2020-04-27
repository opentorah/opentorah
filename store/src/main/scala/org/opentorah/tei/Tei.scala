package org.opentorah.tei

import org.opentorah.entity.{EntityReference, EntityType}
import org.opentorah.xml.{ContentType, Element, ToXml, Xml}
import scala.xml.{Elem, Node}

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

  def references: Seq[EntityReference] = {
    val lookInto: Seq[Node] =
      getAbstract.getOrElse(Seq.empty) ++
      correspDesc.map(_.xml).getOrElse(Seq.empty) ++
      body.xml

    titleStmt.references ++ EntityReference.from(lookInto)
  }

  def addressee: Option[EntityReference] =
    EntityReference.from(correspDesc.map(_.xml).getOrElse(Seq.empty))
      .find(name => (name.entityType == EntityType.Person) && name.role.contains("addressee"))

  /////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>""" + "\n" +
}

object Tei extends Element[Tei](
  elementName = "TEI",
  contentType = ContentType.Elements,
  parser = for {
    teiHeader <- TeiHeader.required
    text <- Text.required
  } yield new Tei(
    teiHeader,
    text
  )
) with ToXml[Tei] {

  type Transformer = Tei => Tei

  override def toXml(value: Tei): Elem =
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      {Xml.removeNamespace(TeiHeader.toXml(value.teiHeader))}
      {Xml.removeNamespace(Text.toXml(value.text))}
    </TEI>

  def apply(body: Seq[Node]): Tei = new Tei(
    teiHeader = TeiHeader(),
    text = new Text(
      lang = None,
      new Body.Value(body)
    )
  )
}
