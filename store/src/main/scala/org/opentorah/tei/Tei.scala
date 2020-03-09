package org.opentorah.tei

import org.opentorah.reference.Reference
import org.opentorah.xml.{ContentType, Descriptor, XmlUtil}
import scala.xml.{Elem, Node}

final case class Tei(
  teiHeader: TeiHeader,
  text: Text
) {
  def titleStmt: TitleStmt = teiHeader.fileDesc.titleStmt
  val correspDesc: Option[CorrespDesc] = teiHeader.profileDesc.flatMap(_.correspDesc)
  def getAbstract: Option[Seq[Node]] = teiHeader.profileDesc.flatMap(_.documentAbstract.map(_.xml))
  def creationDate: Option[Date] = teiHeader.profileDesc.flatMap(_.creation.map(_.date))
  def languages: Seq[Language] = teiHeader.profileDesc.flatMap(_.langUsage).toSeq.flatMap(_.languages)
  val body: Body = text.body
  val pbs: Seq[Pb] = body.xml.flatMap(Pb.descendants)

  def references: Seq[Reference] = {
    val lookInto: Seq[Node] =
      getAbstract.getOrElse(Seq.empty) ++
      correspDesc.map(_.xml).getOrElse(Seq.empty) ++
      body.xml

    titleStmt.references ++ lookInto.flatMap(Reference.all)
  }

  /////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>""" + "\n" +
}

object Tei extends Descriptor[Tei](
  elementName = "TEI",
  contentType = ContentType.Elements,
  parser = for {
    teiHeader <- TeiHeader.required
    text <- Text.required
  } yield new Tei(
    teiHeader,
    text
  )
) {

  def apply(
    publisher: Seq[Node],
    availabilityStatus: String,
    availability: Seq[Node],
    sourceDesc: Seq[Node],
    body: Seq[Node]
  ): Tei = new Tei(
    teiHeader = TeiHeader(
      fileDesc = FileDesc(
        publicationStmt = PublicationStmt(
          publisher,
          availability = Availability(
            availabilityStatus,
            availability
          )
        ),
        sourceDesc
      )
    ),
    text = Text(body)
  )

  override def toXml(value: Tei): Elem =
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      {XmlUtil.removeNamespace(TeiHeader.toXml(value.teiHeader))}
      {XmlUtil.removeNamespace(Text.toXml(value.text))}
    </TEI>
}
