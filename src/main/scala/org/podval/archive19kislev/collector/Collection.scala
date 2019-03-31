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

  private val documents: Seq[Document] = {
    val names: Seq[String] = Collection.listNames(teiDirectory, ".xml", Page.checkBase)

    val namesWithSiblings: Seq[(String, (Option[String], Option[String]))] = {
      val documentOptions: Seq[Option[String]] = names.map(Some(_))
      val prev = None +: documentOptions.init
      val next = documentOptions.tail :+ None
      names.zip(prev.zip(next))
    }

    for ((name, (prev, next)) <- namesWithSiblings) yield {
      val file: File = new File(teiDirectory, name + ".xml")
      new Document(Xml.load(file), name, prev, next)
    }
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
    val imageNames: Seq[String] = Collection.listNames(facsimilesDirectory, ".jpg", Page.check)
    val orphanImages: Seq[String] = (imageNames.toSet -- pages.map(_.name).toSet).toSeq.sorted
    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
  }

  // TODO check order

  def writeIndex(): Unit = {
    check()

    Collection.writeTo(collectionDirectory, "index.md", Seq(
      "title" -> title,
      "layout" -> "collection"
    ))(
      Collection.table.toMarkdown(documents) ++
        (if (missingPages.isEmpty) Seq.empty else Seq(
          "",
          s"Отсутствуют фотографии страниц: ${missingPages.mkString(" ")}"
        ))
    )

    for (document <- documents) {
      Collection.writeTo(documentsDirectory, s"${document.name}.html",
        Seq("layout" -> "document") ++ document.navigation
      )(Seq.empty)

      Collection.writeTo(documentsDirectory, s"${document.name}-facs.html", Seq(
        "layout" -> "facsimile",
        "images" -> document.pages.filter(_.isPresent).map(_.name).mkString("[", ", ", "]")
      ) ++ document.navigation
      )(Seq.empty)
    }

    println(s"--- $directoryName ---")

    for (document <- documents) {
      val people = document.people.filter(_.ref.isEmpty)
        .filterNot(_.name == "Арье-Лейб Дубинский")
        .filterNot(_.name == "Ифрах Абрамов")
        .filterNot(_.name == "Борух Горкин")
        .filterNot(_.name == "Mendel Feller")

      val places = document.places.filter(_.ref.isEmpty)
      val organizations = document.organizations.filter(_.ref.isEmpty)
      if (people.nonEmpty || places.nonEmpty || organizations.nonEmpty) println(document.name)
      printNames("people", people)
      printNames("places", places)
      printNames("organizations", organizations)
    }

    println()
    println("References")
    val references: Set[String] = documents.flatMap { document =>
      (document.people ++ document.places ++ document.organizations)
        .filter(_.ref.isDefined)
        .map(_.ref.get)
        .toSet
    }.toSet
    println(references.toSeq.sorted.mkString("\n"))
  }

  private def printNames(what: String, names: Seq[Name]): Unit = {
    val empty = names.filter(_.ref.isEmpty)
    if (empty.nonEmpty) println(s"- $what -")
    for (name <- empty) {
      println(s"  ${name.name}")
    }
  }
}

object Collection {
  private val docsDirectory: File = new File("docs").getAbsoluteFile

  private val documentsDirectoryName: String = "documents"

  private val collections: Seq[Collection] = Seq(
    new Collection("dubnov", "Дубнов"),
    new Collection("archive", "Архив")
  )

  def main(args: Array[String]): Unit = collections.foreach(_.writeIndex())

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
        content ++
        Seq("")
      writer.write(result.mkString("\n"))
    } finally {
      writer.close()
    }
  }

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

  private def listNames(directory: File, extension: String, check: String => Unit): Seq[String] = {
    val result = directory.listFiles.toSeq.map(_.getName).filter(_.endsWith(extension)).map(_.dropRight(extension.length))
    //    result.foreach(check)
    result.sorted
  }

  private def documentPath(document: Document): String =
    s"$documentsDirectoryName/${document.name}.html"
}
