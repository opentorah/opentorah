package org.opentorah.collector

import java.net.URL
import org.opentorah.store.{By, Selector, Store, Urls}
import org.opentorah.tei.Title
import org.opentorah.util.Collections
import org.opentorah.xml.Parser
import zio.ZIO

abstract class Collection(
  inheritedSelectors: Seq[Selector],
  urls: Urls,
  element: Store.Inline
) extends Store.FromElement(inheritedSelectors, urls, element) {

  def pageType: Page.Type

  override val by: Option[By.FromElement[Document]] = Some(By.fromElement(
    selectors,
    urls = urls.inline,
    element = element.by.get,
    creator = new Collection.DocumentBy(_, _, _)
  ))

  def documents: Seq[Document] = by.get.stores

  val parts: Seq[Collection.Part] = Collection.getParts(
    descriptors = by.get.element.stores.filter(_.isInstanceOf[Store.Inline]).map(_.asInstanceOf[Store.Inline]),
    documents
  )

  def findDocumentByName(documentName: String): Option[(Document, TeiHolder)] =
    documents.find(_.findTeiHolderByName(documentName).isDefined)
      .map(document => document -> document.findTeiHolderByName(documentName).get)

  // TODO do it in Store for all of them
  val siblings: Map[Document, (Option[Document], Option[Document])] =
    Collections.prevAndNext(documents).toMap
}

object Collection {

  final class Part(
    val title: Option[Title.Value],
    val documents: Seq[Document]
  )

  final class DocumentBy(
    inheritedSelectors: Seq[Selector],
    urls: Urls,
    inline: By.Inline
  ) extends By.FromElement[Document](inheritedSelectors, urls, inline) {

    protected def storeCreator: Store.Creator[Document] =
      throw new IllegalArgumentException("Documents can not be loaded inline.")

    override protected def loadFromDirectory(
      fileNames: Seq[String],
      fileInDirectory: String => URL
    ): Seq[Parser[Document]] = {
      val namesWithLang: Seq[(String, Option[String])] = fileNames.map(splitLang)

      val translations: Map[String, Seq[String]] = Collections.mapValues(namesWithLang
        .filter(_._2.isDefined)
        .map(e => (e._1, e._2.get))
        .groupBy(_._1))(_.map(_._2))

      val names: Seq[String] = namesWithLang.filter(_._2.isEmpty).map(_._1)

      for (name <- names) yield ZIO.succeed(new Document(
        inheritedSelectors,
        urls.inline,
        fileInDirectory,
        name,
        languages = translations.getOrElse(name, Seq.empty)
      ))
    }
  }

  private def splitLang(name: String): (String, Option[String]) = {
    val dash: Int = name.lastIndexOf('-')
    if ((dash == -1) || (dash != name.length-3)) (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))
  }

  private def getParts(
    descriptors: Seq[Store.Inline],
    documents: Seq[Document]
  ): Seq[Part] = {

    @scala.annotation.tailrec
    def splitParts(
      result: Seq[Part],
      descriptors: Seq[Store.Inline],
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
      descriptor: Store.Inline,
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
