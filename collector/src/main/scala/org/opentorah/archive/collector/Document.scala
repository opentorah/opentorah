package org.opentorah.archive.collector

import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.{Name, Names}
import org.opentorah.store.{By, Selector, Store}
import org.opentorah.tei.Tei
import org.opentorah.xml.Parser

final class Document(
  inheritedSelectors: Seq[Selector],
  baseUrl: URL,
  fileInDirectory: String => URL,
  val name: String,
  val languages: Seq[String]
) extends Store(inheritedSelectors, None, baseUrl) {

  override val by: Option[By[TeiHolder]] =
    Some(new Document.TeiBy(selectors, baseUrl, fileInDirectory, name, languages))

  def tei: Tei = by.get.stores.head.tei

  override def names: Names = new Names(Seq(Name(name)))

  override def references: Seq[EntityReference] = Seq.empty

  def pages(pageType: Page.Type): Seq[Page] =
    for (pb <- tei.pbs) yield pageType(
      n = pb.n,
      facs = pb.facs
    )
}

object Document {

  // TODO to avoid name concatenations and aloow clearer separation between main document and translation,
  // switch to an extended holder that has language code...
  // TODO check correspondence with the Tei language element.
  final class TeiBy(
    inheritedSelectors: Seq[Selector],
    baseUrl: URL,
    fileInDirectory: String => URL,
    name: String,
    languages: Seq[String]
  ) extends By[TeiHolder](inheritedSelectors, baseUrl) {

    override def selector: Selector = selectorByName("language") // TODO hard-coded...

    override protected def load: Seq[Parser[TeiHolder]] = {
      val names: Seq[String] = name +: (for (language <- languages) yield s"$name-$language")
      for (fileName <- names) yield {
        val fromUrl: URL = fileInDirectory(fileName)
        for {
          tei <- Tei.parse(fromUrl)
        } yield new TeiHolder(
          selectors,
          fromUrl,
          fileName,
          tei
        )
      }
    }
  }
}
