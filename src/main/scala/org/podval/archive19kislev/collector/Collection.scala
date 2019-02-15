package org.podval.archive19kislev.collector

import java.io.File

import scala.xml.Elem


final class Collection(
  val directoryName: String,
  val title: String
) {
  val collectionDirectory = new File(Collection.docsDirectory, directoryName)
  val teiDirectory = new File(collectionDirectory, "documents")
  val facsimilesDirectory = new File(collectionDirectory, "facsimiles")


  // Read

  val imageNames: Seq[String] = listNames(facsimilesDirectory, ".jpg", Name.check)


  val documents: Seq[Document] = {
    val names = listNames(teiDirectory, ".xml", Name.checkBase)
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
    val file = new File(collectionDirectory, "index.html")
    Xml.print(toHtml, file)
  }


  def toHtml: Elem = {
    <html>
      <head>
        <link rel="stylesheet" href={"../css/index.css"} type="text/css"/>
        <title>{title}</title>
      </head>
      <body>
        <div class="title">{title}</div>
        <div>
          <table id="documents-table">
            <thead>
              {Document.indexTableHeader}
            </thead>
            <tbody>
              {documents.flatMap(_.indexTableRows)}
            </tbody>
          </table>

          {if (missingPages.nonEmpty) <p>Отсутствуют фотографии страниц: {missingPages.mkString(" ")}</p>}
        </div>
      </body>
    </html>
  }
}

object Collection {
  val docsDirectory: File = new File("docs").getAbsoluteFile

  def writeIndex(directoryName: String, title: String): Unit =
    new Collection(directoryName, title).writeIndex()
}
