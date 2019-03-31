package org.podval.archive19kislev.collector

import Xml.Ops
import scala.xml.Elem

final class Document(xml: Elem, val name: String, val prev: Option[String], val next: Option[String]) {
  override def toString: String = name

  val navigation: Seq[(String, String)] = documentName("self", name) ++
    prev.map(prev => documentName("prev", prev)).getOrElse(Seq.empty) ++
    next.map(next => documentName("next", next)).getOrElse(Seq.empty)

  private def documentName(what: String, name: String): Seq[(String, String)] = Seq(what -> s"'$name'")

  private[this] val tei: Elem = Xml.open(xml, "TEI")
  private[this] val teiHeader: Elem = tei.oneChild("teiHeader")
  private[this] val fileDesc: Elem = teiHeader.oneChild("fileDesc")
  private[this] val titleStmt: Elem = fileDesc.oneChild("titleStmt")
  private[this] val profileDesc: Elem = teiHeader.oneChild("profileDesc")
  private[this] val body: Elem = tei.oneChild("text").oneChild("body")

  val (partTitle: Option[String], title: Option[String], subTitle: Option[String]) = {
    val titles = titleStmt.elemsFilter("title")
    def getTitle(name: String): Option[String] =
      titles.find(_.getAttribute("type") == name).map(text)

    (getTitle("part"), getTitle("main").flatMap(optionize), getTitle("sub"))
  }

  def author: Option[String] = optionize(titleStmt.oneChild("author").text)

  def transcriber: Option[String] =
    titleStmt.elemsFilter("editor").find(_.attributeOption("role").contains("transcriber")).map(_.text)

  def publicationDate: Option[String] = optionize(fileDesc.oneChild("publicationStmt").oneChild("date").text)

  def date: Option[String] =
    profileDesc.optionalChild("creation").map(_.oneChild("date").getAttribute("when"))

  def description: Option[String] =
    profileDesc.optionalChild("abstract").map(text).orElse(title)

  def language: Option[String] =
    profileDesc.oneChild("langUsage").elems("language").map(_.getAttribute("ident")).headOption

  private[this] def optionize(what: String): Option[String] = if (what.isEmpty || (what == "?")) None else Some(what)

  val pages: Seq[Page] = for {
    pb <- body.descendants("pb")
    name = pb.getAttribute("n")
    isPresent = pb.attributeOption("facs").isDefined
  } yield Page(name, isPresent, this)

  def people: Seq[Name] = names("persName").filter(_.name != "?")
  def places: Seq[Name] = names("placeName")
  def organizations: Seq[Name] = names("orgName")

  def addressee: Option[String] = people.find(_.role.contains("addressee")).map(_.name)

  private def text(e: Elem): String = (e.child map (_.text)).mkString(" ")
  private def names(what: String): Seq[Name] = tei.descendants(what).map(Name(_, this))
}
