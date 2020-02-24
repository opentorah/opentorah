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
  )
) {

  def tei(head: Option[Node], content: Seq[Node]): Elem =
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <teiHeader>
        <fileDesc>
          <titleStmt/>
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
          <sourceDesc><p>Facsimile</p></sourceDesc>
        </fileDesc>
      </teiHeader>
      <text>
        <body>
          {head.fold[Seq[Node]](Seq.empty)(head => Seq(<head>{head}</head>))}
          {content}
        </body>
      </text>
    </TEI>
}
