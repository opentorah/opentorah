package org.podval.archive19kislev.collector

import java.io.File

final class Collection(
  val directoryName: String,
  val title: String
) {
  private val collectionDirectory = new File(Main.docsDirectory, directoryName)
  private val teiDirectory = new File(collectionDirectory, Main.teiDirectoryName)
  private val facsimilesDirectory = new File(collectionDirectory, Main.facsimilesDirectoryName)
  private val documentsDirectory = new File(collectionDirectory, Main.documentsDirectoryName)

  // Read

  val documents: Seq[Document] = {
    val names: Seq[String] = Files.listNames(teiDirectory, ".xml", Page.checkBase)

    val namesWithSiblings: Seq[(String, (Option[String], Option[String]))] = {
      val documentOptions: Seq[Option[String]] = names.map(Some(_))
      val prev = None +: documentOptions.init
      val next = documentOptions.tail :+ None
      names.zip(prev.zip(next))
    }

    for ((name, (prev, next)) <- namesWithSiblings)
    yield new Document(Xml.load(teiDirectory, name), name, prev, next)
  }

  val pages: Seq[Page] = documents.flatMap(_.pages)

  val missingPages: Seq[String] = pages.filterNot(_.isPresent).map(_.displayName)

  /// Check consistency

  private def check(): Unit = {
    // Check for duplicates
    val name2page = collection.mutable.Map[String, Page]()
    for (page <- pages) {
      // TODO allow duplicates in consecutive documents
      //      if (name2page.contains(page.name)) throw new IllegalArgumentException(s"Duplicate page: ${page.name}")
      name2page.put(page.name, page)
    }

    // Check that all the images are accounted for
    val imageNames: Seq[String] = Files.listNames(facsimilesDirectory, ".jpg", Page.check)
    val orphanImages: Seq[String] = (imageNames.toSet -- pages.map(_.name).toSet).toSeq.sorted
    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
  }

  // TODO check order

  def writeIndex(): Unit = {
    check()

    val missing =
      if (missingPages.isEmpty) Seq.empty
      else Seq("", s"Отсутствуют фотографии страниц: ${missingPages.mkString(" ")}")

    Files.write(collectionDirectory, "index.md", Seq(
      "title" -> title,
      "layout" -> "collection"
    ))(Collection.table.toMarkdown(documents) ++ missing)

    for (document <- documents) {
      def documentName(what: String, name: String): Seq[(String, String)] = Seq(what -> s"'$name'")

      val navigation: Seq[(String, String)] = documentName("self", document.name) ++
        document.prev.map(prev => documentName("prev", prev)).getOrElse(Seq.empty) ++
        document.next.map(next => documentName("next", next)).getOrElse(Seq.empty)

      Files.write(documentsDirectory, s"${document.name}.html", Seq(
        "layout" -> "document",
        "tei" -> s"'../${Main.teiDirectoryName}/${document.name}.xml'"
      ) ++ navigation
      )(Seq.empty)

      Files.write(documentsDirectory, s"${document.name}-facs.html", Seq(
        "layout" -> "facsimile",
        "images" -> document.pages.filter(_.isPresent).map(_.name).mkString("[", ", ", "]")
      ) ++ navigation
      )(Seq.empty)
    }
  }
}

object Collection {

  private val table: Table[Document] = new Table[Document](
    _.partTitle.toSeq.map(partTitle =>  s"""<span class="part-title">$partTitle</span>"""),
    ("Описание", _.description.getOrElse("?")),
    ("Дата", _.date.getOrElse("")),
    ("Кто", _.author.getOrElse("")),
    ("Кому", _.addressee.getOrElse("")),
    ("Язык", _.language.getOrElse("")),
    ("Документ", document => Link(document.name, documentPath(document))),
    ("Страницы", document =>
      for (page <- document.pages) yield Link(
        text = page.displayName,
        url = documentPath(document) + s"#p${page.name}",
        cssClass = Some(if (page.isPresent) "page" else "missing-page")
      )),
    ("Расшифровка", _.transcriber.getOrElse(""))
  )

  private def documentPath(document: Document): String =
    s"${Main.documentsDirectoryName}/${document.name}.html"
}
