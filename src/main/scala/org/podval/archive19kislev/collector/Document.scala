package org.podval.archive19kislev.collector

import Xml.Ops
import scala.xml.Elem

final class Document(
  val collection: Collection,
  val name: String,
  val prev: Option[String],
  val next: Option[String]
) extends DocumentLike(collection.teiDirectory, name) {

  override def url: String = collection.documentUrl(name)

  private[this] val titleStmt: Elem = fileDesc.oneChild("titleStmt")

  val (partTitle: Option[Elem], title: Option[Elem], subTitle: Option[Elem]) = {
    val titles: Seq[Elem] = titleStmt.elemsFilter("title")
    def getTitle(name: String): Option[Elem] =
      titles.find(_.getAttribute("type") == name)

    (getTitle("part"), getTitle("main"), getTitle("sub"))
  }

  def author: Option[Elem] =
    titleStmt.optionalChild("author")

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

  override def persNames: Seq[Name] = names(teiDescendants("persName"))
  override def placeNames: Seq[Name] = names(teiDescendants("placeName"))
  override def orgNames: Seq[Name] = names(teiDescendants("orgName"))

  def addressee: Option[Name] =
    persNames.find(_.role.contains("addressee"))
}
