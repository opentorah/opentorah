package org.digitaljudaica.archive.collector

import java.io.File

import Xml.Ops

import scala.xml.Elem

final class Document(
  layout: Layout,
  collectionDirectoryName: String,
  teiDirectory: File,
  val name: String,
  prev: Option[String],
  next: Option[String],
  val translations: Seq[String],
  pageType: Page.Type
) extends DocumentLike(teiDirectory, name) {

  override def url: String = layout.documentUrl(collectionDirectoryName, name)

  private[this] val titleStmt: Elem = fileDesc.oneChild("titleStmt")

  val (title: Option[Elem], subTitle: Option[Elem]) = {
    val titles: Seq[Elem] = titleStmt.elemsFilter("title")
    def getTitle(name: String): Option[Elem] =
      titles.find(_.getAttribute("type") == name)

    (getTitle("main"), getTitle("sub"))
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

  val pages: Seq[Page] = for (pb <- body.descendants("pb")) yield pageType(
    name = pb.getAttribute("n"),
    isPresent = pb.attributeOption("facs").isDefined
  )

  override def persNames: Seq[Name] = namesOf(Entity.Person)
  override def placeNames: Seq[Name] = namesOf(Entity.Place)
  override def orgNames: Seq[Name] = namesOf(Entity.Organization)

  private def namesOf(entity: Entity): Seq[Name] = names(teiDescendants(entity.nameElement))

  def addressee: Option[Name] =
    persNames.find(_.role.contains("addressee"))

  def writeWrappers(docsDirectory: File, facsDirectory: File): Unit = {
    def quote(what: String): String = s"'$what'"

    val navigation: Seq[(String, String)] =
      prev.map(prev => Seq("prev" -> quote(prev))).getOrElse(Seq.empty) ++
      Seq("self" -> quote(name)) ++
      next.map(next => Seq("next" -> quote(next))).getOrElse(Seq.empty)

    def writeTeiWrapper(name: String, lang: Option[String]): Unit = {
      val nameWithLang: String = lang.fold(name)(lang => name + "-" + lang)

      Util.writeYaml(docsDirectory, nameWithLang, layout = "document", Seq(
        "tei" -> s"'../${layout.teiDirectoryName}/$nameWithLang.xml'",
        "facs" -> s"'../${layout.facsDirectoryName}/$name.html'"
      ) ++ (if (lang.isDefined || translations.isEmpty) Seq.empty else Seq("translations" -> translations.mkString("[", ", ", "]")))
        ++ navigation
      )
    }

    // TEI wrapper(s)
    writeTeiWrapper(name, None)
    for (lang <- translations) writeTeiWrapper(name, Some(lang))

    // Facsimile viewer
    Util.writeYaml(facsDirectory, name, layout = "facsimile", Seq(
      "images" -> pages.filter(_.isPresent).map(_.name).mkString("[", ", ", "]")
    ) ++ navigation
    )
  }
}
