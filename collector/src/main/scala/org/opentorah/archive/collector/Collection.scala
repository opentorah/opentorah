package org.opentorah.archive.collector

import java.net.URL
import org.opentorah.store.{By, ByElement, Store, StoreElement}
import org.opentorah.tei.Tei
import org.opentorah.util.{Collections, Files}
import org.opentorah.xml.{From, Parser}

final class Collection(
  parent: Option[Store],
  url: URL,
  element: StoreElement.Inline
) extends Store.FromElement(parent, url, element) {

  private def byElement: ByElement = element.by.get

  override val by: Option[Collection.DocumentBy] =
    Some(new Collection.DocumentBy(this, byElement))

  def documents: Seq[Document] = by.get.stores

  val parts: Seq[Collection.Part] = Collection.getParts(
    descriptors = byElement.stores.map(_.asInstanceOf[StoreElement.Inline]),
    documents
  )
}

object Collection {

  final class Part(
    val title: Option[StoreElement.Title.Value],
    val documents: Seq[Document]
  )

  final class DocumentBy(
    store: Store,
    element: ByElement
  ) extends By.FromElement(store, element) {

    override val stores: Seq[Document] = getDocuments(
      parent = store,
      element,
      filesWithExtensions(store.url, "xml")
    )
  }

  private def getDocuments(
    parent: Store,
    element: ByElement,
    filesWithExtensions: Seq[String]
  ): Seq[Document] = {

    def fileInDirectory(name: String): URL =
      Files.fileInDirectory(Files.subdirectory(parent.url, element.directory.get), name + ".xml")

    val namesWithLang: Seq[(String, Option[String])] =
      filesWithExtensions.map(splitLang)

    val translations: Map[String, Seq[String]] = Collections.mapValues(namesWithLang
      .filter(_._2.isDefined)
      .map(e => (e._1, e._2.get))
      .groupBy(_._1))(_.map(_._2))

    val names: Seq[String] = namesWithLang.filter(_._2.isEmpty).map(_._1)

    def readTei(url: URL): Tei = Parser.parseDo(Tei.parse(From.url(url)))

    for (name <- names) yield {
      val url: URL = fileInDirectory(name)
      new Document(
        parent = Some(parent),
        url,
        tei = readTei(url),
        name,
        translations = translations.getOrElse(name, Seq.empty).map(language =>
          language -> readTei(fileInDirectory(name + "-" + language))
        ).toMap
      )
    }
  }

  private def splitLang(name: String): (String, Option[String]) = {
    val dash: Int = name.lastIndexOf('-')
    if ((dash == -1) || (dash != name.length-3)) (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))
  }

  private def getParts(
    descriptors: Seq[StoreElement.Inline],
    documents: Seq[Document]
  ): Seq[Part] = {

    @scala.annotation.tailrec
    def splitParts(
      result: Seq[Part],
      descriptors: Seq[StoreElement.Inline],
      documents: Seq[Document]
    ): (Seq[Part], Seq[Document]) = descriptors match {
      case Nil => (result, documents)

      case d1 :: d2 :: ds =>
        val (partDocuments: Seq[Document], tail: Seq[Document]) = documents.span(_.name != d2.from.get)
        splitParts(result :+ mkPart(d1, partDocuments), d2 :: ds, tail)

      case d1 :: Nil =>
        (result :+ mkPart(d1, documents), Seq.empty)
    }

    def mkPart(
      descriptor: StoreElement.Inline,
      documents: Seq[Document]
    ): Part = {
      if (documents.isEmpty) throw new IllegalArgumentException("No documents for Part!")
      if (documents.head.name != descriptor.from.get) throw new IllegalArgumentException("Incorrect 'from' document")
      new Part(descriptor.title, documents)
    }

    if (descriptors.isEmpty) Seq(new Part(None, documents)) else {
      val (result: Seq[Part], leftOver: Seq[Document]) =
        splitParts(Seq.empty, descriptors, documents)

      if (leftOver.nonEmpty) throw new IllegalArgumentException(
        "Documents left over: " + leftOver.mkString(", ") + ".")

      result
    }
  }
}
