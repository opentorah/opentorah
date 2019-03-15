package org.podval.archive19kislev.collector

import java.io.{BufferedWriter, File, FileWriter}

import scala.xml.Elem

final class Collection(
  val directoryName: String,
  val title: String
) {
  val collectionDirectory = new File(Collection.docsDirectory, directoryName)
  val teiDirectory = new File(collectionDirectory, "documents")
  val facsimilesDirectory = new File(collectionDirectory, "facsimiles")

  // Read

  val imageNames: Seq[String] = listNames(facsimilesDirectory, ".jpg", Page.check)

  val documents: Seq[Document] = {
    val names = listNames(teiDirectory, ".xml", Page.checkBase)
    for (name <- names) yield {
      val file = new File(teiDirectory, name + ".xml")
      new Document(Xml.load(file), name)
    }
  }

  val pages: Seq[Page] = documents.flatMap(_.pages)

  val missingPages: Seq[String] = pages.filterNot(_.isPresent).map(_.displayName)

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

  val documentsWithSiblings: Seq[(Document, (Option[Document], Option[Document]))] = {
    val documentOptions: Seq[Option[Document]] = documents.map(Some(_))
    val prev = None +: documentOptions.init
    val next = documentOptions.tail :+ None
    documents.zip(prev.zip(next))
  }

  // TODO check order

  /// Generate HTML

  def writeIndex(): Unit = {
//    val html = new File(collectionDirectory, "index.html")
//    Xml.print(toHtml, html)
    val md = new File(collectionDirectory, "list.md")
    val writer: BufferedWriter = new BufferedWriter(new FileWriter(md))
    try {
      writer.write(toMarkdown)
    } finally {
      writer.close()
    }
  }

  def toHtml: Elem = {
    <html>
      <head>
        <link rel="stylesheet" href={"../assets/list.css"} type="text/css"/>
        <title>{title}</title>
      </head>
      <body>
        <div class="title">{title}</div>
        <div>
          <table id="documents-table">
            <thead>
              <tr>{
                for (column <- Collection.columns) yield <th class={column.cssClass}>{column.heading}</th>
              }</tr>
            </thead>
            <tbody>{ documents.flatMap { document =>
              document.partTitle.toSeq.map { partTitle =>
                <tr>
                  <td class="part-title" colspan={Collection.columns.length.toString}>{partTitle}</td>
                </tr>
              } ++ Seq(
                <tr>{
                  for (column <- Collection.columns) yield <td>{Collection.toXml(column.value(document))}</td>
                }</tr>
              )
            }}
            </tbody>
          </table>

          {if (missingPages.nonEmpty) <p>Отсутствуют фотографии страниц: {missingPages.mkString(" ")}</p>}
        </div>
      </body>
    </html>
  }

  def toMarkdown: String = {
    (Seq[String](
      "---",
      s"title: $title",
      "layout: page",
      "list: true",
      "---",
      toString(for (column <- Collection.columns) yield column.heading),
      toString(for (_      <- Collection.columns) yield "---")
    ) ++ documents.flatMap { document =>
      document.partTitle.toSeq.map { partTitle =>
        val result = s"""<span class="part-title">$partTitle</span>"""
        toString(Seq(result))
      } ++ Seq[String](
        toString(for (column <- Collection.columns) yield Collection.toMarkdown(column.value(document)))
      )} ++ (if (missingPages.isEmpty) Seq.empty else Seq(
      "",
      s"Отсутствуют фотографии страниц: ${missingPages.mkString(" ")}",
      ""
    ))).mkString("\n")
  }

  private def toString(cells: Seq[String]): String = cells.mkString("| ", " | ", " |")
}

object Collection {
  private val docsDirectory: File = new File("docs").getAbsoluteFile

  def main(args: Array[String]): Unit = {
    writeIndex("dubnov", "Дубнов")
    writeIndex("archive", "Архив")
  }

  private def writeIndex(directoryName: String, title: String): Unit =
    new Collection(directoryName, title).writeIndex()

  private final case class Column[T](
    heading: String,
    cssClass: String,
    value: Document => T
  )

  private final case class Link(
    text: String,
    url: String,
    cssClass: Option[String] = None
  ) {
    def toXml: Elem =
      if (cssClass.isEmpty) <a href={url}>{text}</a>
      else <a class={cssClass.get} href={url}>{text}</a>

    def toMarkdown: String =
      if (cssClass.isEmpty) s"[$text]($url)"
      else s"""<a class="${cssClass.get}" href="$url">$text</a>"""
  }

  private val columns = Seq(
    Column[String]("Описание", "description", _.title.getOrElse("?")),
    Column[String]("Дата", "date", _.date.getOrElse("")),
    Column[String]("Автор", "author", _.author.getOrElse("")),
    Column[String]("Язык", "language", _.language.getOrElse("")),
    Column[Link]("Документ", "document",
      document => {
        val name = document.name
        Link(name, s"documents/$name.xml")
      }),
    Column[Seq[Link]]("Страницы", "pages",
      document => for (page <- document.pages) yield Link(
        text = page.displayName,
        url = s"documents/${page.document.name}.xml#p${page.name}",
        cssClass = Some(if (page.isPresent) "page" else "missing-page")
      )),
    Column[String]("Текст", "isTranscribed",
      document => if (document.isTranscribed) "да" else "нет")
  )

  private def toXml(value: AnyRef): AnyRef = value match {
    case link: Link => link.toXml
    case links: Seq[Link] => links.map(_.toXml)
    case other => other.toString
  }

  private def toMarkdown(value: AnyRef): String = value match {
    case link: Link => link.toMarkdown
    case links: Seq[Link] => links.map(_.toMarkdown).mkString(" ")
    case other => other.toString
  }
}
