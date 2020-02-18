package org.digitaljudaica.archive.collector.tei

import java.io.File
import cats.implicits._
import org.digitaljudaica.xml.Ops._
import org.digitaljudaica.xml.{From, Parser, Xml}

import scala.xml.{Elem, Node}

class Tei private(
  val teiHeader: Elem,
  val body: Elem
) {
  val fileDesc: Elem = teiHeader.oneChild("fileDesc")
  val publicationStmt: Elem = fileDesc.oneChild("publicationStmt")
  val publicationDate: Option[Elem] = publicationStmt.optionalChild("date")
  val titleStmt: Option[Elem] = fileDesc.optionalChild("titleStmt")
  val titles: Seq[Elem] = titleStmt.fold[Seq[Elem]](Seq.empty)(titleStmt => titleStmt.elemsFilter("title"))
  def getTitle(name: String): Option[Elem] = titles.find(_.getAttribute("type") == name)
  val author: Option[Elem] = titleStmt.flatMap(_.optionalChild("author"))
  val editors: Seq[Editor] = titleStmt.fold[Seq[Elem]](Seq.empty)(_.elemsFilter("editor")).map(Editor.apply)
  val profileDesc: Option[Elem] = teiHeader.optionalChild("profileDesc")
  val getAbstract: Option[Elem] = profileDesc.flatMap(_.optionalChild("abstract"))
  val creation: Option[Elem] = profileDesc.flatMap(_.optionalChild("creation"))
  val date: Option[String] = creation.map(_.oneChild("date").getAttribute("when"))
  val langUsage: Option[Elem] = profileDesc.flatMap(_.optionalChild("langUsage"))
  val languages: Seq[Elem] = langUsage.fold[Seq[Elem]](Seq.empty)(_.elems("language"))
  val languageIdents: Seq[String] = languages.map(_.getAttribute("ident"))
  val pbs: Seq[Pb] = org.digitaljudaica.xml.Ops.descendants(body, "pb").map(Pb.apply)

/////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>""" + "\n" +
}

object Tei {

  val topElement: String = "TEI"

  private val parser: Parser[Tei] = for {
    _ <- Xml.checkName(topElement)
    mustBeTeiHeader <-  Xml.next.elementName
    _ <- Parser.check(mustBeTeiHeader.contains("teiHeader"), "No teiHeader!")
    teiHeader <- Xml.next.element // TODO add name-checking flavour with the name parameter!
    body <- Xml.element.elements.required("text", for {
      lang <- Xml.attribute.optional("xml:lang")
      mustBeBody <-  Xml.next.elementName
      _ <- Parser.check(mustBeBody.contains("body"), "No body!")
      result <- Xml.next.element
    } yield result) // TODO unfold Xml.element!
  } yield new Tei(
    teiHeader,
    body
  )

  def load(directory: File, fileName: String): Tei = From.file(directory, fileName).elements.parseDo(parser)

  def bodyParser[A](parser: Parser[A]): Parser[A] = for {
    _ <- Xml.checkName(topElement)
    _ <- Xml.element.elements.required("teiHeader", Xml.allElements)
    result <- Xml.element.elements.required("text", Xml.element.elements.required("body", parser))
  } yield result

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
