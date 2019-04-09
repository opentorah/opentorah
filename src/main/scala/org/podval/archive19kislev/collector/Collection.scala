package org.podval.archive19kislev.collector

import java.io.File

import scala.xml.{Node, Text}

final class Collection(docsDirectory: File, val directoryName: String, val title: String) {
  override def toString: String = directoryName

  private val collectionDirectory = new File(docsDirectory, directoryName)
  val teiDirectory = new File(collectionDirectory, Collection.teiDirectoryName)
  private val facsimilesDirectory = new File(collectionDirectory, Collection.facsimilesDirectoryName)
  private val documentsDirectory = new File(collectionDirectory, Collection.documentsDirectoryName)
  private val viewersDirectory = new File(collectionDirectory, Collection.viewersDirectoryName)

  def documentUrl(name: String): String =
    "/" + directoryName + "/" + Collection.documentsDirectoryName + "/" + name + ".html"

  val documents: Seq[Document] = {
    val names: Seq[String] = Collection.listNames(teiDirectory, ".xml", Page.checkBase)

    val namesWithSiblings: Seq[(String, (Option[String], Option[String]))] = {
      val documentOptions: Seq[Option[String]] = names.map(Some(_))
      val prev = None +: documentOptions.init
      val next = documentOptions.tail :+ None
      names.zip(prev.zip(next))
    }

    for ((name, (prev, next)) <- namesWithSiblings)
    yield new Document(this, name, prev, next)
  }

  private val pages: Seq[Page] = documents.flatMap(_.pages)

  private val missingPages: Seq[String] = pages.filterNot(_.isPresent).map(_.displayName)

  /// Check consistency
  check()

  private def check(): Unit = {
    // Check for duplicates
    val name2page = collection.mutable.Map[String, Page]()
    for (page <- pages) {
      // TODO allow duplicates in consecutive documents
      //      if (name2page.contains(page.name)) throw new IllegalArgumentException(s"Duplicate page: ${page.name}")
      name2page.put(page.name, page)
    }

    // Check that all the images are accounted for
    val imageNames: Seq[String] = Collection.listNames(facsimilesDirectory, ".jpg", Page.check)
    val orphanImages: Seq[String] = (imageNames.toSet -- pages.map(_.name).toSet).toSeq.sorted
    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
  }

  // TODO check order

  def writeIndex(): Unit = {
    val content =
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
            {Collection.table.toTei(documents)}
            {if (missingPages.isEmpty) Seq.empty
             else <p>Отсутствуют фотографии страниц: {missingPages.mkString(" ")}</p>}
          </body>
        </text>
      </TEI>


    // Index
    Xml.write(collectionDirectory, "index", content)

    // Index wrapper
    Collection.write(collectionDirectory, "index.html", Seq(
      "title" -> title,
      "layout" -> "tei",
      "tei" -> "index.xml",
      "wide" -> "true",
      "target" -> "collectionViewer"
    ))
  }

  def writeWrappers(): Unit = {
    for (document <- documents) {
      def quote(what: String): String = s"'$what'"

      val navigation: Seq[(String, String)] =
        document.prev.map(prev => Seq("prev" -> quote(prev))).getOrElse(Seq.empty) ++
        Seq("self" -> quote(document.name)) ++
        document.next.map(next => Seq("next" -> quote(next))).getOrElse(Seq.empty)

      // TEI wrapper
      Collection.write(documentsDirectory, s"${document.name}.html", Seq(
        "layout" -> "tei",
        "tei" -> s"'../${Collection.teiDirectoryName}/${document.name}.xml'"
      ) ++ navigation
      )

      // Facsimile viewer
      Collection.write(viewersDirectory, s"${document.name}.html", Seq(
        "layout" -> "facsimile",
        "images" -> document.pages.filter(_.isPresent).map(_.name).mkString("[", ", ", "]")
      ) ++ navigation
      )
    }
  }
}

object Collection {
  val teiDirectoryName: String = "tei"
  val facsimilesDirectoryName: String = "facsimiles"
  val documentsDirectoryName: String = "documents" // wrappers for TEI XML
  val viewersDirectoryName: String = "facs" // facsimile viewers

  private val table: Table[Document] = new Table[Document](
    _.partTitle.toSeq.map(partTitle =>  <span rendition="part-title">{partTitle.child}</span>),
    Column.elem("Описание", "description", _.description),
    Column.string("Дата", "date", _.date),
    Column.elem("Кто", "author", _.author),
    new Column("Кому", "addressee",  _.addressee.fold[Seq[Node]](Text(""))(addressee =>
      <persName ref={addressee.ref.map("#" + _).orNull}>{addressee.name}</persName>)),
    Column.string("Язык", "language", _.language),
    new Column("Документ", "document", document =>
      <ref target={documentPath(document)} role="documentViewer">{document.name}</ref>),
    new Column("Страницы", "pages", document => for (page <- document.pages) yield
      <ref target={documentPath(document) + s"#p${page.name}"} role="documentViewer"
           rendition={if (page.isPresent) "page" else "missing-page"}>{page.displayName}</ref>),
    Column.elem("Расшифровка", "transcriber", _.transcriber)
  )

  private def documentPath(document: Document): String =
    s"${Collection.documentsDirectoryName}/${document.name}.html"

  private def listNames(directory: File, extension: String, check: String => Unit): Seq[String] = {
    val result = directory.listFiles.toSeq.map(_.getName).filter(_.endsWith(extension)).map(_.dropRight(extension.length))
    //    result.foreach(check)
    result.sorted
  }

  private def write(
    directory: File,
    fileName: String,
    yaml: Seq[(String, String)]
  ): Unit = {
    val result: Seq[String] =
      Seq("---") ++
        (for ((name, value) <- yaml) yield name + ": " + value) ++
        Seq("---") ++
        Seq("")

    Util.write(
      directory,
      fileName,
      result
    )
  }
}
