package org.podval.archive19kislev.collector

import java.io.{BufferedWriter, File, FileWriter}

final class Collection(
  directoryName: String,
  title: String
) {
  private val collectionDirectory = new File(Collection.docsDirectory, directoryName)
  private val teiDirectory = new File(collectionDirectory, "tei")
  private val facsimilesDirectory = new File(collectionDirectory, "facsimiles")
  private val documentsDirectory = new File(collectionDirectory, Collection.documentsDirectoryName)

  // Read

  private val imageNames: Seq[String] = listNames(facsimilesDirectory, ".jpg", Page.check)

  private val documents: Seq[Document] = {
    val names: Seq[String] = listNames(teiDirectory, ".xml", Page.checkBase)
    for (name <- names) yield {
      val file: File = new File(teiDirectory, name + ".xml")
      new Document(Xml.load(file), name)
    }
  }

  private val documentsWithSiblings: Seq[(Document, (Option[Document], Option[Document]))] = {
    val documentOptions: Seq[Option[Document]] = documents.map(Some(_))
    val prev = None +: documentOptions.init
    val next = documentOptions.tail :+ None
    documents.zip(prev.zip(next))
  }

  private val pages: Seq[Page] = documents.flatMap(_.pages)

  private val missingPages: Seq[String] = pages.filterNot(_.isPresent).map(_.displayName)

  private def listNames(directory: File, extension: String, check: String => Unit): Seq[String] = {
    val result = directory.listFiles.toSeq.map(_.getName).filter(_.endsWith(extension)).map(_.dropRight(extension.length))
//    result.foreach(check)
    result.sorted
  }

  /// Check consistency

  {
    // Check for duplicates
    val name2page = collection.mutable.Map[String, Page]()
    for (page <- pages) {
// TODO allow duplicates in consecutive documents
//      if (name2page.contains(page.name)) throw new IllegalArgumentException(s"Duplicate page: ${page.name}")
      name2page.put(page.name, page)
    }

    // Check that all the images are accounted for
    val orphanImages: Seq[String] = (imageNames.toSet -- pages.map(_.name).toSet).toSeq.sorted
    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
  }

  // TODO check order

  def writeIndex(): Unit = {
    Collection.writeTo(collectionDirectory, "index.md", Seq(
      "title" -> title,
      "layout" -> "collection"
    ))(
      Collection.table.toMarkdown(documents) ++
        (if (missingPages.isEmpty) Seq.empty else Seq(
          "",
          s"Отсутствуют фотографии страниц: ${missingPages.mkString(" ")}"
        )) ++
        Seq("")
    )

    for ((document, (prev, next)) <- documentsWithSiblings) {
      val navigation: Seq[(String, String)] = documentName("self", document) ++
        prev.map(prev => documentName("prev", prev)).getOrElse(Seq.empty) ++
        next.map(next => documentName("next", next)).getOrElse(Seq.empty)

      Collection.writeTo(documentsDirectory, s"${document.name}.html",
        Seq("layout" -> "document") ++ navigation
      )(Seq.empty)

      Collection.writeTo(documentsDirectory, s"${document.name}-facs.html",
        Seq("layout" -> "facsimile") ++ navigation
      )(
        for (page <- document.pages.filter(_.isPresent)) yield {
          val name: String = page.name
          s"""<img id="p$name" src="../facsimiles/$name.jpg" alt="facsimile for page $name">"""
        }
      )
    }
  }

  private def documentName(what: String, document: Document): Seq[(String, String)] =
    Seq(what -> s"'${document.name}'")
}

object Collection {
  private val docsDirectory: File = new File("docs").getAbsoluteFile

  private val documentsDirectoryName: String = "documents"

  def main(args: Array[String]): Unit = {
    new Collection("dubnov", "Дубнов").writeIndex()
    new Collection("archive", "Архив").writeIndex()
  }

  private def writeTo(
    directory: File,
    fileName: String,
    yaml: Seq[(String, String)]
  )(content: Seq[String]): Unit = {
    directory.mkdirs()
    val file = new File(directory, fileName)
    val writer: BufferedWriter = new BufferedWriter(new FileWriter(file))
    try {
      val result =
        Seq("---") ++
        (for ((name, value) <- yaml) yield name + ": " + value) ++
        Seq("---") ++
        content
      writer.write(result.mkString("\n"))
    } finally {
      writer.close()
    }
  }

  private val table: Table[Document] = new Table[Document](
    _.partTitle.toSeq.map(partTitle =>  s"""<span class="part-title">$partTitle</span>"""),
    ("Описание", _.description.getOrElse("?")),
    ("Дата", _.date.getOrElse("")),
    ("Автор", _.author.getOrElse("")),
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
    s"$documentsDirectoryName/${document.name}.html"
}
