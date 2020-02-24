package org.digitaljudaica.tei

import org.digitaljudaica.reference.Reference
import org.digitaljudaica.xml.{ContentType, Descriptor}
import org.digitaljudaica.xml.Ops._

import scala.xml.{Elem, Node}

// TODO rework by introducing various TEI-related classes (TitleStmt etc.)
final case class Tei(
  teiHeader: TeiHeader,
  text: Text
) {
  private val titleStmt = teiHeader.fileDesc.titleStmt

  private val titles: Seq[Elem] = titleStmt.fold[Seq[Elem]](Seq.empty)(titleStmt => titleStmt.xml.elemsFilter("title"))
  def getTitle(name: String): Option[Elem] = titles.find(_.getAttribute("type") == name)
  val author: Option[Elem] = titleStmt.flatMap(_.xml.optionalChild("author"))
  val editors: Seq[Editor] = titleStmt.fold[Seq[Elem]](Seq.empty)(_.xml.elemsFilter("editor")).map(Editor.apply)
  val getAbstract: Option[Elem] = teiHeader.profileDesc.flatMap(_.documentAbstract.map(_.xml))

  private val creation: Option[Elem] = teiHeader.profileDesc.flatMap(_.creation.map(_.xml))
  val date: Option[String] = creation.map(_.oneChild("date").getAttribute("when"))

  private val langUsage: Option[Elem] = teiHeader.profileDesc.flatMap(_.langUsage.map(_.xml))
  private val languages: Seq[Elem] = langUsage.fold[Seq[Elem]](Seq.empty)(_.elems("language"))
  val languageIdents: Seq[String] = languages.map(_.getAttribute("ident"))

  val body: Body = text.body
  val pbs: Seq[Pb] = Pb.descendants(body.xml)

  def references: Seq[Reference] = (
    titleStmt.toSeq.map(_.xml) ++ getAbstract.toSeq ++
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
