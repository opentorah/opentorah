package org.digitaljudaica.archive.collector

import java.io.File
import scala.xml.{Elem, Node, Text}
import Table.Column
import Xml.Ops

final class Collection(
  layout: Layout,
  directory: File,
  xml: Elem,
  val includeInNavigation: Boolean,
  isBook: Boolean,
  errors: Errors
) {
  def directoryName: String = directory.getName

  def reference: String = xml.optionalChild("reference").map(_.text).getOrElse(directoryName)

  def title: String = xml.optionalChild("title").map(_.text).getOrElse(reference)

  def description: Seq[Elem] = xml.oneChild("abstract").elements

  private def pageType: Page.Type = if (isBook) Page.Book else Page.Manuscript

  private val parts: Seq[Part] = Part.splitParts(xml.elemsFilter("part"), getDocuments)

  private val documents: Seq[Document] = parts.flatMap(_.documents)

  def references: Seq[Reference] = documents.flatMap(_.references)

  private val pages: Seq[Page] = documents.flatMap(_.pages)

  private val missingPages: Seq[String] = pages.filterNot(_.isPresent).map(_.displayName)

  /// Check consistency
  checkPages()

  private def checkPages(): Unit = {
    // Check for duplicates
    val name2page = collection.mutable.Map[String, Page]()
    for (page <- pages) {
      // allow duplicates in consecutive documents
      //   if (name2page.contains(page.name)) throw new IllegalArgumentException(s"Duplicate page: ${page.name}")
      name2page.put(page.name, page)
    }

    // Check that all the images are accounted for
    val imageNames: Set[String] =
      Util.filesWithExtensions(
        directory = layout.facsimiles(directory),
        ".jpg"
      )
      .toSet
    imageNames.foreach(name => pageType(name, isPresent = true))

    val usedImages: Set[String] = pages.filter(_.isPresent).map(_.name).toSet
    val orphanImages: Seq[String] = (imageNames -- usedImages).toSeq.sorted
    val missingImages: Seq[String] = (usedImages -- imageNames).toSeq.sorted
    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
    if (missingImages.nonEmpty)
      throw new IllegalArgumentException(s"Missing images: $missingImages")
  }

  private def splitLang(name: String): (String, Option[String]) = {
    val dash: Int = name.lastIndexOf('-')
    if ((dash == -1) || (dash != name.length-3)) (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))
  }

  private def getDocuments: Seq[Document] = {
    val teiDirectory = layout.tei(directory)

    val namesWithLang: Seq[(String, Option[String])] =
      Util.filesWithExtensions(teiDirectory, ".xml").sorted.map(splitLang)

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
      layout,
      collectionDirectoryName = directoryName,
      teiDirectory,
      name,
      prev,
      next,
      translations = translations.getOrElse(name, Seq.empty),
      pageType,
      errors
    )
  }

  def process(): Unit = {
    // Index
    Tei.tei(title, content =
      description ++
        Seq[Elem](Collection.table(layout).toTei(
          parts.flatMap { part =>  part.title.map(Table.Xml).toSeq ++ part.documents.map(Table.Data[Document]) }
        )) ++
        (if (missingPages.isEmpty) Seq.empty
        else Seq(<p>Отсутствуют фотографии {missingPages.length} страниц: {missingPages.mkString(" ")}</p>))
    ).write(directory, "index")

    // Index wrapper
    Util.writeTeiYaml(directory, "index",
      layout = "collection",
      tei = "index.xml",
      title = reference,
      target = "collectionViewer"
    )

    // Wrappers
    val docsDirectory = layout.docs(directory)
    Util.deleteFiles(docsDirectory)
    val facsDirectory = layout.facs(directory)
    Util.deleteFiles(facsDirectory)

    for (document <- documents) document.writeWrappers(docsDirectory, facsDirectory)
  }
}

object Collection {

  private def table(layout: Layout): Table[Document] = new Table[Document](
    Column("Описание", "description", { document: Document =>
      Xml.contentOf(document.description)
    }),

    Column("Дата", "date", { document: Document =>
      document.date.fold[Seq[Node]](Text(""))(value => Text(value))
    }),

    Column("Кто", "author", { document: Document =>
      Xml.contentOf(document.author)
    }),

    Column("Кому", "addressee",  _.addressee.fold[Seq[Node]](Text(""))(addressee =>
      <persName ref={addressee.ref.map("#" + _).orNull}>{addressee.name}</persName>)),

    Column("Язык", "language", { document: Document =>
      val translations: Seq[Elem] = for (translation <- document.translations) yield
        <ref target={layout.documentUrlRelativeToIndex(document.name + "-" + translation)}
             role="documentViewer">{translation}</ref>

      Seq(Text(document.language.getOrElse("?"))) ++ translations
    }),

    Column("Документ", "document", { document: Document =>
      <ref target={layout.documentUrlRelativeToIndex(document.name)}
           role="documentViewer">{document.name}</ref>
    }),

    Column("Страницы", "pages", { document: Document => for (page <- document.pages) yield
      <ref target={layout.documentUrlRelativeToIndex(document.name) + s"#p${page.name}"}
           role="documentViewer"
           rendition={if (page.isPresent) "page" else "missing-page"}>{page.displayName}</ref>
    }),

    Column("Расшифровка", "transcriber", { document: Document =>
      Xml.contentOf(document.transcriber)
    })
  )
}
