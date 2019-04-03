package org.podval.archive19kislev.collector

import Xml.Ops
import scala.xml.Elem

final class Document(xml: Elem, val name: String, val prev: Option[String], val next: Option[String]) {
  override def toString: String = name

  private[this] val tei: Elem = Xml.open(xml, "TEI")
  private[this] val teiHeader: Elem = tei.oneChild("teiHeader")
  private[this] val fileDesc: Elem = teiHeader.oneChild("fileDesc")
  private[this] val titleStmt: Elem = fileDesc.oneChild("titleStmt")
  private[this] val profileDesc: Elem = teiHeader.oneChild("profileDesc")
  private[this] val body: Elem = tei.oneChild("text").oneChild("body")

  val (partTitle: Option[Elem], title: Option[Elem], subTitle: Option[Elem]) = {
    val titles: Seq[Elem] = titleStmt.elemsFilter("title")
    def getTitle(name: String): Option[Elem] =
      titles.find(_.getAttribute("type") == name)

    (getTitle("part"), getTitle("main"), getTitle("sub"))
  }

  def author: Option[Elem] = titleStmt.optionalChild("author")

  def transcriber: Option[Elem] =
    titleStmt.elemsFilter("editor").find(_.attributeOption("role").contains("transcriber"))

  def publicationDate: Option[Elem] =
    fileDesc.oneChild("publicationStmt").optionalChild("date")

  def date: Option[String] =
    profileDesc.optionalChild("creation").map(_.oneChild("date").getAttribute("when"))

  def description: Option[Elem] =
    profileDesc.optionalChild("abstract").orElse(title)

  def language: Option[String] =
    profileDesc.oneChild("langUsage").elems("language").map(_.getAttribute("ident")).headOption

  val pages: Seq[Page] = for {
    pb <- body.descendants("pb")
    name = pb.getAttribute("n")
    isPresent = pb.attributeOption("facs").isDefined
  } yield Page(name, isPresent, this)

  def people: Seq[Name] = names("persName").filter(_.name != "?")
  def places: Seq[Name] = names("placeName")
  def organizations: Seq[Name] = names("orgName")
  def references: Seq[Name] = people ++ places ++ organizations

  def addressee: Option[Name] =
    people.find(_.role.contains("addressee"))

  private def names(what: String): Seq[Name] = tei.descendants(what).map(Name.apply)
}
