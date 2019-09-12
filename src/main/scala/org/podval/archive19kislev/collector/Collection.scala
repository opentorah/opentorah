package org.podval.archive19kislev.collector

import java.io.File

import scala.xml.{Elem, Node, Text}
import Xml.Ops

final class Collection(directory: File, xml: Elem) {
  def directoryName: String = directory.getName

  def includeInNavigation: Boolean = xml.attributeOption("includeInNavigation").contains("true")

  def reference: String = xml.optionalChild("reference").map(_.text).getOrElse(directoryName)

  def title: String = xml.optionalChild("title").map(_.text).getOrElse(reference)

  def description: Seq[Elem] = xml.oneChild("description").elements

  private val parts: Seq[Part] = Part.splitParts(xml.elemsFilter("part"), getDocuments)

  private val documents: Seq[Document] = parts.flatMap(_.documents)

  def names: Seq[Name] = documents.flatMap(_.names)

  private val pages: Seq[Page] = documents.flatMap(_.pages)

  private val missingPages: Seq[String] = pages.filterNot(_.isPresent).map(_.displayName)

  /// Check consistency
  checkPages()

  private def checkPages(): Unit = {
    // Check for duplicates
    val name2page = collection.mutable.Map[String, Page]()
    for (page <- pages) {
      // TODO allow duplicates in consecutive documents
      //      if (name2page.contains(page.name)) throw new IllegalArgumentException(s"Duplicate page: ${page.name}")
      name2page.put(page.name, page)
    }

    // Check that all the images are accounted for
    val imageNames: Seq[String] = Collection.listNames(
      directory = Layout.facsimiles(directory),
      ".jpg",
      Page.check
    )
    val orphanImages: Seq[String] = (imageNames.toSet -- pages.map(_.name).toSet).toSeq.sorted
    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
  }

  // TODO check order

  private def getDocuments: Seq[Document] = {
    def splitLang(name: String): (String, Option[String]) = {
      val dash: Int = name.lastIndexOf('-')
      if ((dash == -1) || (dash != name.length-3)) (name, None)
      else (name.substring(0, dash), Some(name.substring(dash+1)))
    }

    val teiDirectory = Layout.tei(directory)

    val namesWithLang: Seq[(String, Option[String])] =
      Collection.listNames(teiDirectory, ".xml", Page.checkBase).map(splitLang)

    val translations: Map[String, Seq[String]] = namesWithLang
      .filter(_._2.isDefined)
      .map(e => (e._1, e._2.get))
      .groupBy(_._1)
      .view.mapValues(_.map(_._2)).toMap

    val names: Seq[String] = namesWithLang.filter(_._2.isEmpty).map(_._1)

    val namesWithSiblings: Seq[(String, (Option[String], Option[String]))] = if (names.isEmpty) Seq.empty else {
      val documentOptions: Seq[Option[String]] = names.map(Some(_))
      val prev = None +: documentOptions.init
      val next = documentOptions.tail :+ None
      names.zip(prev.zip(next))
    }

    for ((name, (prev, next)) <- namesWithSiblings) yield new Document(
      collectionDirectoryName = directoryName,
      teiDirectory,
      name,
      prev,
      next,
      translations = translations.getOrElse(name, Seq.empty)
    )
  }

  private def index: Elem =
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <teiHeader>
        <fileDesc>
          <publicationStmt>
            <publisher><ptr target="www.alter-rebbe.org"/></publisher>
            <availability status="free">
              <licence><ab><ref n="license" target="http://creativecommons.org/licenses/by/4.0/">
                Creative Commons Attribution 4.0 International License </ref></ab></licence>
            </availability>
          </publicationStmt>
          <sourceDesc><p>Facsimile</p></sourceDesc>
        </fileDesc>
        <profileDesc><calendarDesc><calendar xml:id="julian"><p>Julian calendar</p></calendar></calendarDesc></profileDesc>
      </teiHeader>
      <text>
        <body>
          <!-- <title>{title}</title> -->
          {description}
          {Collection.table.toTei(
            parts.flatMap { part =>  part.title.map(Table.Xml).toSeq ++ part.documents.map(Table.Data[Document]) }
          )}
          {if (missingPages.isEmpty) Seq.empty
           else <p>Отсутствуют фотографии {missingPages.length} страниц: {missingPages.mkString(" ")}</p>}
        </body>
      </text>
    </TEI>

  def process(): Unit = {
    // Index
    index.write(directory, "index")

    // Index wrapper
    Util.write(directory, "index.html", Seq(
      "reference" -> reference,
      "title" -> title,
      "layout" -> "tei",
      "tei" -> "index.xml",
      "wide" -> "true",
      "target" -> "collectionViewer"
    ))

    // Wrappers
    val docsDirectory = Layout.docs(directory)
    Util.deleteFiles(docsDirectory)
    val facsDirectory = Layout.facs(directory)
    Util.deleteFiles(facsDirectory)

    for (document <- documents) document.writeWrappers(docsDirectory, facsDirectory)
  }
}

object Collection {

  private val table: Table[Document] = new Table[Document](
    Column.elem("Описание", "description", _.description),

    Column.string("Дата", "date", _.date),

    Column.elem("Кто", "author", _.author),

    new Column("Кому", "addressee",  _.addressee.fold[Seq[Node]](Text(""))(addressee =>
      <persName ref={addressee.ref.map("#" + _).orNull}>{addressee.name}</persName>)),

    new Column("Язык", "language", { document: Document =>
      val translations: Seq[Elem] =
        for (translation <- document.translations) yield
          <ref target={Layout.documentUrlRelativeToIndex(document.name + "-" + translation)}
               role="documentViewer">{translation}</ref>

      Seq(Text(document.language.getOrElse("?"))) ++ translations
    }),

    new Column("Документ", "document", document =>
      <ref target={Layout.documentUrlRelativeToIndex(document.name)} role="documentViewer">{document.name}</ref>),

    new Column("Страницы", "pages", document => for (page <- document.pages) yield
      <ref target={Layout.documentUrlRelativeToIndex(document.name) + s"#p${page.name}"} role="documentViewer"
           rendition={if (page.isPresent) "page" else "missing-page"}>{page.displayName}</ref>),

    Column.elem("Расшифровка", "transcriber", _.transcriber)
  )

  private def listNames(directory: File, extension: String, check: String => Unit): Seq[String] = {
    val result = Util.filesWithExtensions(directory, extension)
    //    result.foreach(check)
    result.sorted
  }
}
