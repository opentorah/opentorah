package org.podval.archive19kislev.collector

import org.podval.archive19kislev.collector.Xml.Ops
import scala.xml.Elem

final class Document(xml: Elem, val name: String) {
  override def toString: String = name

  private[this] val tei: Elem = Xml.open(xml, "TEI")
  private[this] val teiHeader: Elem = tei.oneChild("teiHeader")
  private[this] val fileDesc: Elem = teiHeader.oneChild("fileDesc")
  private[this] val titleStmt: Elem = fileDesc.oneChild("titleStmt")
  private[this] val profileDesc: Elem = teiHeader.oneChild("profileDesc")

  val (partTitle: Option[String], title: Option[String], subTitle: Option[String]) = {
    val titles = titleStmt.elemsFilter("title")
    def getTitle(name: String): Option[String] =
      titles.find(_.getAttribute("type") == name).map(text)

    (getTitle("part"), getTitle("main").flatMap(optionize), getTitle("sub"))
  }

  private def text(e: Elem): String = (e.child map (_.text)).mkString(" ")

  def author: Option[String] = optionize(titleStmt.oneChild("author").text)
  def transcriber: Option[String] = titleStmt.elemsFilter("editor")
    .find(_.attributeOption("role").contains("transcriber")).map(_.text)

  def publicationDate: Option[String] = optionize(fileDesc.oneChild("publicationStmt").oneChild("date").text)

  def langUsage: Elem = profileDesc.oneChild("langUsage")
  def date: Option[String] = profileDesc.optionalChild("creation")
      .map(_.oneChild("date").getAttribute("when"))

  def profileAbstract: Option[Elem] = profileDesc.optionalChild("abstract")
  def description: Option[String] = profileAbstract.map(text).orElse(title)

  def languages: Seq[String] = langUsage.elems("language").map(_.getAttribute("ident"))
  def language: Option[String] = languages.headOption

  def content: Seq[Elem] = tei.oneChild("text").oneChild("body").elems

  private[this] def optionize(what: String): Option[String] = if (what.isEmpty || (what == "?")) None else Some(what)

  val pages: Seq[Page] = {
    val pbs: Seq[Elem] = content.flatMap(_ \\ "pb").filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])
    for {
      pb <- pbs
      name = pb.getAttribute("n")
      isPresent = pb.attributeOption("facs").isDefined
    } yield Page(name, isPresent, this)
  }
}
