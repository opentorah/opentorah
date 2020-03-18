package org.opentorah.archive.collector

import java.net.URL
import org.opentorah.metadata.{Language, Name, Names}
import org.opentorah.reference.Reference
import org.opentorah.store.{By, FilesList, Nameds, Path, Selector}
import org.opentorah.tei.Tei
import org.opentorah.util.{Collections, Files}
import org.opentorah.xml.{Attribute, Element, From, Parser, RawXml, Text}
import scala.xml.Node

// TODO add toXml and pretty-print collection descriptors;
// TODO fish references out of the collection descriptors too!
final class Collection private(
  val url: URL,
  val documentSelector: Selector.Named,
  val name: String,
  val publish: Boolean,
  val archive: Option[String],
  val prefix: Option[String],
  val number: Option[Int],
  val archiveCase: String,
  val reference: String,
  val title: Node,
  val caseAbstract: Collection.Abstract.Value,
  val description: Seq[Node],
  val parts: Seq[Part]
) extends org.opentorah.store.Store with Ordered[Collection] {

  override def toString: String = reference

  override def names: Names = new Names(Seq(
    Name(reference, Language.Russian),
    Name(name, Language.English)
  ))

  override def nameds: Option[Nameds] = None

  override def selectors: Seq[Selector] = Seq.empty

  val documents: Seq[Document] = parts.flatMap(_.documents)

  private val byDocument = new By {
    override def selector: Selector.Named = documentSelector

    override def stores: Seq[org.opentorah.store.Store] = documents
  }

  override def by: Option[By] = Some(byDocument)

  override def references(at: Path): Seq[Reference] = byDocument.references(at)

  override def compare(that: Collection): Int = {
    val archiveComparison: Int = Collections.compare(archive, that.archive)
    if (archiveComparison != 0) archiveComparison else {
      val prefixComparison: Int = Collections.compare(prefix, that.prefix)
      if (prefixComparison != 0) prefixComparison else {
        number.getOrElse(0).compare(that.number.getOrElse(0))
      }
    }
  }

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
    documentSelector: Selector.Named,
    from: From
  ): Parser[Collection] = new Element[Collection](
    elementName = "collection",
    parser = parser(from.url.get, documentSelector, collectionName)
  ).parse(from)

  private def parser(
    url: URL,
    documentSelector: Selector.Named,
    collectionName: String
  ): Parser[Collection] = for {
    isBook <- Attribute("isBook").boolean.orFalse
    publish <- Attribute("publish").boolean.orFalse
    directory <- Attribute("directory").optional
    list <- Attribute("list").optional
    archive <- Text("archive").optional
    prefix <- Text("prefix").optional
    number <- Text("number").int.optional
    titleRaw <- Title.parsable.optional
    caseAbstract <- Abstract.parsable.required
    notes <- Notes.parsable.optional
    description = Seq(<span>{caseAbstract.xml}</span>) ++ notes.map(_.xml).getOrElse(Seq.empty)
    // TODO swap parts and notes; remove notes wrapper element; simplify parts; see how to generalize parts...
    partDescriptors <- Part.parsable.all
  } yield {
    val archiveCase = prefix.getOrElse("") + number.map(_.toString).getOrElse("")
    val reference: String = archive.fold(archiveCase)(archive => archive + " " + archiveCase)

    val documents: Seq[Document] = getDocuments(
      baseUrl = Files.subdirectory(url, directory.getOrElse(s"$collectionName")),
      list = Files.fileInDirectory(url, list.getOrElse(s"$collectionName-list-generated.xml")),
      isBook
    )

    new Collection(
      url,
      documentSelector,
      collectionName,
      publish,
      archive,
      prefix,
      number,
      archiveCase,
      reference,
      title = titleRaw.fold[Node](scala.xml.Text(reference))(title => <title>{title.xml}</title>),
      caseAbstract,
      description,
      parts = Part.Descriptor.splitParts(partDescriptors, documents)
    )
  }

  private def getDocuments(
    baseUrl: URL,
    list: URL,
    isBook: Boolean
  ): Seq[Document] = {
    val fileNames = FilesList.filesWithExtensions(baseUrl, list, "xml")
    val namesWithLang: Seq[(String, Option[String])] = fileNames.map(splitLang)

    val translations: Map[String, Seq[String]] = Collections.mapValues(namesWithLang
      .filter(_._2.isDefined)
      .map(e => (e._1, e._2.get))
      .groupBy(_._1))(_.map(_._2))

    val names: Seq[String] = namesWithLang.filter(_._2.isEmpty).map(_._1)

    for (name <- names) yield {
      val url: URL = Files.fileInDirectory(baseUrl, name + ".xml")
      new Document(
        url,
        pageType = if (isBook) Page.Book else Page.Manuscript,
        tei = Parser.parseDo(Tei.parse(From.url(url))),
        name,
        translations = translations.getOrElse(name, Seq.empty)
      )
    }
  }

  private def splitLang(name: String): (String, Option[String]) = {
    val dash: Int = name.lastIndexOf('-')
    if ((dash == -1) || (dash != name.length-3)) (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))
  }
}
