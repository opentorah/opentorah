package org.digitaljudaica.tei

import org.digitaljudaica.reference.Reference
import org.digitaljudaica.xml.{ContentType, Descriptor}
import scala.xml.Node

final case class Tei(
  teiHeader: TeiHeader,
  text: Text
) {
  def titleStmt: TitleStmt = teiHeader.fileDesc.titleStmt
  val correspDesc: Option[CorrespDesc] = teiHeader.profileDesc.flatMap(_.correspDesc)
  def getAbstract: Option[Seq[Node]] = teiHeader.profileDesc.flatMap(_.documentAbstract.map(_.xml))
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
  contentParser = for {
    teiHeader <- TeiHeader.required
    text <- Text.required
  } yield new Tei(
    teiHeader,
    text
  ),
  toXml = (value: Tei) =>
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      {TeiHeader.toXml(value.teiHeader)}
      {Text.toXml(value.text)}
    </TEI>
) {
  def tei(head: Option[Node], content: Seq[Node]): Tei = Tei(
    teiHeader = TeiHeader(
      fileDesc = new FileDesc(
        titleStmt = TitleStmt(Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty),
        editionStmt =  None,
        extent = None,
        publicationStmt = PublicationStmt(
          publisher = Some(Publisher(<ptr target="www.alter-rebbe.org"/>)),
          availability = Some(Availability(
            status = Some("free"), xml =
              <licence>
                <ab>
                  <ref n="license" target="http://creativecommons.org/licenses/by/4.0/">
                    Creative Commons Attribution 4.0 International License</ref>
                </ab>
              </licence>))
        ),
        seriesStmt = None,
        notesStmt = None,
        sourceDesc = SourceDesc(<p>Facsimile</p>)
      ),
      encodingDesc = None,
      profileDesc = None,
      xenoData = None,
      revisionDesc = None
    ),
    text = Text(
      lang = None,
      body = Body(head.fold[Seq[Node]](Seq.empty)(head => Seq(<head>{head}</head>)) ++ content)
    )
  )
}
