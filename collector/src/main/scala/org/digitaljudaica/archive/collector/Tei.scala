package org.digitaljudaica.archive.collector

import java.io.File
import org.digitaljudaica.xml.Ops._
import org.digitaljudaica.xml.From
import scala.xml.{Elem, Node}

class Tei(val tei: Elem) {
  tei.check(Tei.topElement)

  val teiHeader: Elem = tei.oneChild("teiHeader")
  val fileDesc: Elem = teiHeader.oneChild("fileDesc")
  val publicationStmt: Elem = fileDesc.oneChild("publicationStmt")
  val publicationDate: Option[Elem] = publicationStmt.optionalChild("date")
  val titleStmt: Option[Elem] = fileDesc.optionalChild("titleStmt")
  val titles: Seq[Elem] = titleStmt.fold[Seq[Elem]](Seq.empty)(titleStmt => titleStmt.elemsFilter("title"))
  def getTitle(name: String): Option[Elem] = titles.find(_.getAttribute("type") == name)
  val author: Option[Elem] = titleStmt.flatMap(_.optionalChild("author"))
  val editors: Seq[Elem] = titleStmt.fold[Seq[Elem]](Seq.empty)(_.elemsFilter("editor"))
  val profileDesc: Option[Elem] = teiHeader.optionalChild("profileDesc")
  val getAbstract: Option[Elem] = profileDesc.flatMap(_.optionalChild("abstract"))
  val creation: Option[Elem] = profileDesc.flatMap(_.optionalChild("creation"))
  val date: Option[String] = creation.map(_.oneChild("date").getAttribute("when"))
  val langUsage: Option[Elem] = profileDesc.flatMap(_.optionalChild("langUsage"))
  val languages: Seq[Elem] = langUsage.fold[Seq[Elem]](Seq.empty)(_.elems("language"))
  val languageIdents: Seq[String] = languages.map(_.getAttribute("ident"))
  val body: Elem = tei.oneChild("text").oneChild("body")
  val pbs: Seq[Elem] = org.digitaljudaica.xml.Ops.descendants(body, "pb")

/////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>""" + "\n" +
}

object Tei {
  val topElement: String = "TEI"

  def load(directory: File, fileName: String): Tei =
    new Tei(From.file(directory, fileName).loadDo.check(topElement))

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
