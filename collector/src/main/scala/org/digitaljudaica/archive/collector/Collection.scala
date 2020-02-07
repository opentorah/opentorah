package org.digitaljudaica.archive.collector

import java.io.File
import org.digitaljudaica.metadata.Xml.Ops
import org.digitaljudaica.util.{Files, Collections}
import org.digitaljudaica.archive.collector.reference.Reference
import Table.Column
import scala.xml.{Elem, Node, Text}

final class Collection(
  layout: Layout,
  directory: File,
  xml: Elem
) extends CollectionLike with Ordered[Collection] {

  def directoryName: String = directory.getName

  override def toString: String = directoryName

  val isBook: Boolean = xml.attributeOption("isBook").contains("true")

  val publish: Boolean = xml.attributeOption("publish").contains("true")

  val teiDirectory: File = layout.tei(directory)

  def archive: Option[String] = xml.optionalChild("archive").map(_.text)

  def prefix: Option[String] = xml.optionalChild("prefix").map(_.text)

  def number: Option[Int] = xml.optionalChild("number").map(_.text.toInt)

  def archiveCase: String = prefix.getOrElse("") + number.map(_.toString).getOrElse("")

  override def reference: String = archive.fold(archiveCase)(archive => archive + " " + archiveCase)

  def caseAbstract: Elem = xml.oneChild("abstract")

  override def compare(that: Collection): Int = {
    val archiveComparison: Int = compare(archive, that.archive)
    if (archiveComparison != 0) archiveComparison else {
      val prefixComparison: Int = compare(prefix, that.prefix)
      if (prefixComparison != 0) prefixComparison else {
        number.getOrElse(0).compare(that.number.getOrElse(0))
      }
    }
  }

  // TODO where is this in the standard library?
  private def compare(a: Option[String], b: Option[String]): Int = {
    if (a.isEmpty && b.isEmpty) 0
    else if (a.isEmpty) -1
    else if (b.isEmpty) 1
    else a.get.compare(b.get)
  }

  def title: Node = xml.optionalChild("title").getOrElse(Text(reference))

  def description: Seq[Node] = xml.oneChild("abstract").child ++
    xml.optionalChild("notes").map(notes => notes.child).getOrElse(Seq.empty)

  def pageType: Page.Type = if (isBook) Page.Book else Page.Manuscript

  private val parts: Seq[Part] = Part.splitParts(xml.elemsFilter("part"), getDocuments)

  private val documents: Seq[Document] = parts.flatMap(_.documents)

  def references: Seq[Reference] = documents.flatMap(_.references)

  private val pages: Seq[Page] = documents.flatMap(_.pages)

  private val missingPages: Seq[String] = pages.filterNot(_.isPresent).map(_.displayName)

  /// Check consistency
  checkPages()

  private def checkPages(): Unit = {
    // TODO with images on a separate website (facsimiles.alter-rebbe.org), this has to be re-worked...
//    // Check that all the images are accounted for
//    val imageNames: Set[String] =
//      Util.filesWithExtensions(
//        directory = layout.facsimiles(directory),
//        ".jpg"
//      )
//      .toSet
//    imageNames.foreach(name => pageType(name, isPresent = true))
//
//    val usedImages: Set[String] = pages.filter(_.isPresent).map(_.name).toSet
//    val orphanImages: Seq[String] = (imageNames -- usedImages).toSeq.sorted
//    val missingImages: Seq[String] = (usedImages -- imageNames).toSeq.sorted
//    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
//    if (missingImages.nonEmpty)
//      throw new IllegalArgumentException(s"Missing images: $missingImages")
  }

  private def splitLang(name: String): (String, Option[String]) = {
    val dash: Int = name.lastIndexOf('-')
    if ((dash == -1) || (dash != name.length-3)) (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))
  }

  private def getDocuments: Seq[Document] = {
    val namesWithLang: Seq[(String, Option[String])] =
      Files.filesWithExtensions(teiDirectory, ".xml").sorted.map(splitLang)

    val translations: Map[String, Seq[String]] = Collections.mapValues(namesWithLang
      .filter(_._2.isDefined)
      .map(e => (e._1, e._2.get))
      .groupBy(_._1))(_.map(_._2))

    val names: Seq[String] = namesWithLang.filter(_._2.isEmpty).map(_._1)

    val namesWithSiblings: Seq[(String, (Option[String], Option[String]))] = if (names.isEmpty) Seq.empty else {
      val documentOptions: Seq[Option[String]] = names.map(Some(_))
      val prev = None +: documentOptions.init
      val next = documentOptions.tail :+ None
      names.zip(prev.zip(next))
    }

    for ((name, (prev, next)) <- namesWithSiblings) yield new Document(
      layout,
      collection = this,
      name,
      prev,
      next,
      translations = translations.getOrElse(name, Seq.empty)
    )
  }

  def process(): Unit = {
    // Index
    Util.writeTei(
      directory,
      fileName = "index",
      head = Some(title),
      content = description ++
        Seq[Elem](Collection.table(layout).toTei(
          parts.flatMap { part =>  part.title.map(Table.Xml).toSeq ++ part.documents.map(Table.Data[Document]) }
        )) ++
        (if (missingPages.isEmpty) Seq.empty
        else Seq(<p>Отсутствуют фотографии {missingPages.length} страниц: {missingPages.mkString(" ")}</p>)),
      style = Some("wide"),
      target = "collectionViewer",
      yaml = Seq("documentCollection" -> Util.quote(reference))
    )

    // Wrappers
    val docsDirectory = layout.docs(directory)
    Files.deleteFiles(docsDirectory)
    val facsDirectory = layout.facs(directory)
    Files.deleteFiles(facsDirectory)

    for (document <- documents) document.writeWrappers(docsDirectory, facsDirectory)
  }
}

object Collection {

  private def table(layout: Layout): Table[Document] = new Table[Document](
    Column("Описание", "description", { document: Document =>
      document.description.fold[Seq[Node]](Text(""))(_.withoutNamespace.child)
    }),

    Column("Дата", "date", { document: Document =>
      document.date.fold[Seq[Node]](Text(""))(value => Text(value))
    }),

    Column("Кто", "author", { document: Document =>
      document.author.fold[Seq[Node]](Text(""))(author => multi(author.withoutNamespace.child))
    }),

    Column("Кому", "addressee",  _.addressee.fold[Seq[Node]](Text(""))(addressee =>
      <persName ref={addressee.ref.orNull}>{addressee.name}</persName>)),

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
      <ref target={layout.documentUrlRelativeToIndex(document.name) + s"#p${page.n}"}
           role="documentViewer"
           rendition={if (page.isPresent) "page" else "missing-page"}>{page.displayName}</ref>
    }),

    Column("Расшифровка", "transcriber", { document: Document => multi(document.transcribers.map(_.withoutNamespace))
    })
  )

  private def multi(nodes: Seq[Node]): Seq[Node] = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: ns => Seq(n, Text(", ")) ++ multi(ns)
    case n => n
  }
}
