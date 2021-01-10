package org.opentorah.collector

import java.net.URL
import org.opentorah.metadata.{Name, Names}
import org.opentorah.store.{By, Selector, Store, Urls}
import org.opentorah.tei.{EntityReference, Page, Tei}
import org.opentorah.xml.{Parser, Xml}

final class Document(
  inheritedSelectors: Seq[Selector],
  urls: Urls,
  fileInDirectory: String => URL,
  val name: String,
  val languages: Seq[String]
) extends Store(inheritedSelectors, urls) {

  override val by: Option[By[TeiHolder]] =
    Some(new Document.TeiBy(selectors, urls, fileInDirectory, name, languages))

  override def names: Names = new Names(Seq(Name(name)))

  def teiHolders: Seq[TeiHolder] = by.get.stores

  def tei: Tei = teiHolders.head.tei

  def findTeiHolderByName(documentName: String): Option[TeiHolder] =
    teiHolders.find(_.name == documentName)

  def pages(pageType: Page.Type): Seq[Page] =
    for (pb <- tei.pbs) yield pageType(pb)

  def description: Seq[Xml.Node] =
    tei.getAbstractXml
    // Ignoring the titles:          .orElse(document.tei.titleStmt.titles.headOption.map(_.xml))
    .getOrElse(Seq.empty)

  def date: Seq[Xml.Node] = Xml.mkText(tei.creationDate.map(_.when).getOrElse(""))

  def author: Seq[Xml.Node] = Xml.multi(tei.titleStmt.authors.flatMap(_.xml))

  def addressee: Seq[Xml.Node] =
    Seq(tei.addressee.fold[Xml.Node](Xml.mkText(""))(addressee => EntityReference.required.xml(addressee)))
}

object Document {

  // TODO check correspondence with the Tei language element - and codify the fact that the first
  // TeiHolder is the original: retrieve the languages of translations from tei.text.lang.get of the other ones,
  // remove TeiHolder.language, and move it into the store.
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
