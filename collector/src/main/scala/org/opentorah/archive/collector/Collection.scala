package org.opentorah.archive.collector

import java.io.File
import java.net.URL
import org.opentorah.metadata.Language
import org.opentorah.reference.Reference
import org.opentorah.store.Path
import org.opentorah.tei.Tei
import org.opentorah.util.{Collections, Files}
import org.opentorah.xml.{Attribute, Element, From, Parser, RawXml, Text, XmlUtil}
import Table.Column
import scala.xml.{Elem, Node}

// TODO add toXml and pretty-print collection descriptors;
// TODO fish references out of the collection descriptors too!
final class Collection private(
  val url: URL,
  val path: Path,
  val publish: Boolean,
  val archive: Option[String],
  val prefix: Option[String],
  val number: Option[Int],
  val archiveCase: String,
  val title: Node,
  val caseAbstract: Collection.Abstract.Value,
  val description: Seq[Node],
  val parts: Seq[Part]
) extends Ordered[Collection] {

  def directoryName: String = path.reference(Language.English.toSpec)

  override def compare(that: Collection): Int = {
    val archiveComparison: Int = Collections.compare(archive, that.archive)
    if (archiveComparison != 0) archiveComparison else {
      val prefixComparison: Int = Collections.compare(prefix, that.prefix)
      if (prefixComparison != 0) prefixComparison else {
        number.getOrElse(0).compare(that.number.getOrElse(0))
      }
    }
  }

  val documents: Seq[Document] = parts.flatMap(_.documents)

  def references: Seq[Reference] = documents.flatMap(_.references)

  private val pages: Seq[Page] = documents.flatMap(_.pages)

  val missingPages: Seq[String] = pages.filterNot(_.isPresent).map(_.displayName)

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
}

object Collection {

  object Title extends RawXml("title")
  object Abstract extends RawXml("abstract")
  object Notes extends RawXml("notes")

  def parse(
    collectionName: String,
    from: From
  ): Parser[Collection] = new Element[Collection](
    elementName = "collection",
    parser = parser(collectionName, from.url.get)
  ).parse(from)

  private def parser(
    collectionName: String,
    url: URL
  ): Parser[Collection] = for {
    isBook <- Attribute("isBook").boolean.orFalse
    publish <- Attribute("publish").boolean.orFalse
    archive <- Text("archive").optional
    prefix <- Text("prefix").optional
    number <- Text("number").int.optional
    teiUrlString <- Text("tei").optional
    titleRaw <- Title.parsable.optional
    caseAbstract <- Abstract.parsable.required
    notes <- Notes.parsable.optional
    description = Seq(<span>{caseAbstract.xml}</span>) ++ notes.map(_.xml).getOrElse(Seq.empty)
    // TODO swap parts and notes; remove notes wrapper element; simplify parts; see how to generalize parts...
    partDescriptors <- Part.parsable.all
  } yield {
    val archiveCase = prefix.getOrElse("") + number.map(_.toString).getOrElse("")
    val reference: String = archive.fold(archiveCase)(archive => archive + " " + archiveCase)

    val teiUrl: URL = new URL(url, teiUrlString.getOrElse(s"$collectionName/"))

    val path: Path = Selectors.collectionPath(collectionName, reference)

    new Collection(
      url,
      path,
      publish,
      archive,
      prefix,
      number,
      archiveCase,
      title = titleRaw.fold[Node](scala.xml.Text(reference))(title => <title>{title.xml}</title>),
      caseAbstract,
      description,
      parts = Part.Descriptor.splitParts(partDescriptors, getDocuments(path, teiUrl, isBook))
    )
  }

  private def getDocuments(
    path: Path,
    teiUrl: URL,
    isBook: Boolean
  ): Seq[Document] = {
    val teiDirectory: File = Files.toFile(teiUrl)

    val namesWithLang: Seq[(String, Option[String])] =
      Files.filesWithExtensions(teiDirectory, "xml").sorted.map(splitLang)

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
      url = new URL(teiUrl, name + ".xml"),
      path = path ++ Selectors.documentPath(name),
      pageType = if (isBook) Page.Book else Page.Manuscript,
      tei = Parser.parseDo(Tei.parse(From.file(teiDirectory, name))),
      name,
      prev,
      next,
      translations = translations.getOrElse(name, Seq.empty)
    )
  }

  private def splitLang(name: String): (String, Option[String]) = {
    val dash: Int = name.lastIndexOf('-')
    if ((dash == -1) || (dash != name.length-3)) (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))
  }

  def table(documentUrlRelativeToIndex: String => String): Table[Document] = new Table[Document](
    Column("Описание", "description", { document: Document =>
      document.description.getOrElse(Seq.empty).map(XmlUtil.removeNamespace)
    }),

    Column("Дата", "date", { document: Document =>
      document.date.fold[Seq[Node]](scala.xml.Text(""))(value => scala.xml.Text(value))
    }),

    Column("Кто", "author", { document: Document =>
      multi(document.authors.flatMap(_.map(XmlUtil.removeNamespace)))
    }),

    Column("Кому", "addressee",  _.addressee.fold[Seq[Node]](scala.xml.Text(""))(addressee =>
      <persName ref={addressee.ref.orNull}>{addressee.name}</persName>)),

    Column("Язык", "language", { document: Document =>
      val translations: Seq[Elem] = for (translation <- document.translations) yield
        <ref target={documentUrlRelativeToIndex(document.name + "-" + translation)}
             role="documentViewer">{translation}</ref>

      Seq(scala.xml.Text(document.language.getOrElse("?"))) ++ translations
    }),

    Column("Документ", "document", { document: Document =>
      <ref target={documentUrlRelativeToIndex(document.name)}
           role="documentViewer">{document.name}</ref>
    }),

    Column("Страницы", "pages", { document: Document => for (page <- document.pages) yield
      <ref target={documentUrlRelativeToIndex(document.name) + s"#p${page.n}"}
           role="documentViewer"
           rendition={if (page.isPresent) "page" else "missing-page"}>{page.displayName}</ref>
    }),

    Column("Расшифровка", "transcriber", { document: Document =>
      multi(document.transcribers.map(transcriber => XmlUtil.removeNamespace(org.opentorah.reference.Reference.toXml(transcriber))))
    })
  )

  private def multi(nodes: Seq[Node]): Seq[Node] = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: ns if n.isInstanceOf[Elem] => Seq(n, scala.xml.Text(", ")) ++ multi(ns)
    case n :: ns => Seq(n) ++ multi(ns)
    case n => n
  }
}
