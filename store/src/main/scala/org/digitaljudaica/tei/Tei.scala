package org.digitaljudaica.tei

import org.digitaljudaica.reference.Reference
import org.digitaljudaica.xml.{ContentType, Descriptor}
import scala.xml.{Elem, Node}

final case class Tei(
  teiHeader: TeiHeader,
  text: Text
) {
  def titleStmt: TitleStmt = teiHeader.fileDesc.titleStmt
  def getAbstract: Option[Elem] = teiHeader.profileDesc.flatMap(_.documentAbstract.map(_.xml))
  val body: Body = text.body
  val pbs: Seq[Pb] = Pb.descendants(body.xml)

  def references: Seq[Reference] = titleStmt.references ++ (
    getAbstract.toSeq ++
    teiHeader.profileDesc.toSeq.flatMap(_.correspDesc.toSeq).map(_.xml) ++
    Seq(body.xml)
  ).flatMap(Reference.all)

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
          <publicationStmt>
            <publisher><ptr target="www.alter-rebbe.org"/></publisher>
            <availability status="free">
              <licence>
                <ab>
                  <ref n="license" target="http://creativecommons.org/licenses/by/4.0/">
                    Creative Commons Attribution 4.0 International License </ref>
                </ab>
              </licence>
            </availability>
          </publicationStmt>
        ),
        seriesStmt = None,
        notesStmt = None,
        sourceDesc = SourceDesc(<sourceDesc><p>Facsimile</p></sourceDesc>)
      ),
      encodingDesc = None,
      profileDesc = None,
      xenoData = None,
      revisionDesc = None
    ),
    text = Text(
      lang = None,
      body = Body(
        <body>
          {head.fold[Seq[Node]](Seq.empty)(head => Seq(<head>{head}</head>))}
          {content}
        </body>
      )
    )
  )
}
