package org.opentorah.collector

import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.{Name, Names}
import org.opentorah.store.{By, Selector, Store, Urls}
import org.opentorah.tei.Tei
import org.opentorah.xml.Parser

final class Document(
  inheritedSelectors: Seq[Selector],
  urls: Urls,
  fileInDirectory: String => URL,
  val name: String,
  val languages: Seq[String]
) extends Store(inheritedSelectors, urls) {

  override val by: Option[By[TeiHolder]] =
    Some(new Document.TeiBy(selectors, urls, fileInDirectory, name, languages))

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

  // TODO check correspondence with the Tei language element.
  final class TeiBy(
    inheritedSelectors: Seq[Selector],
    urls: Urls,
    fileInDirectory: String => URL,
    name: String,
    languages: Seq[String]
  ) extends By[TeiHolder](inheritedSelectors, urls) {

    override def selector: Selector = selectorByName("language") // TODO hard-coded...

    override protected def load: Seq[Parser[TeiHolder]] = {
      def teiHolder(name: String, language: Option[String]): Parser[TeiHolder] = {
        val fromUrl: URL = fileInDirectory(name)
        for {
          tei <- Tei.parse(fromUrl)
        } yield new TeiHolder(
          selectors,
          urls = Urls.fromUrl(fromUrl),
          name,
          language,
          tei
        )
      }
      teiHolder(name, None) +:
        (for (language <- languages) yield teiHolder(s"$name-$language", Some(language)))
    }
  }
}
