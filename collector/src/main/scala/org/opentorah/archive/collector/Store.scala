package org.opentorah.archive.collector

import java.net.URL
import org.opentorah.metadata.{Name, Names}
import org.opentorah.reference.Named
import org.opentorah.store.{By, ByElement, Nameds, Selector, StoreElement}
import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.{From, Parser}

final class Store(
  val url: URL,
  element: StoreElement
) extends org.opentorah.store.Store {

  override def names: Names = new Names(Seq(Name("root")))

  override def selectors: Seq[Selector] = element.selectors

  val nameds: Option[Nameds] = Some(new Nameds(element.nameds.get, selectorByName, url))

  final class ByCase(element: ByElement) extends By {
    override def selector: Selector.Named = selectorByName(element.selector).asNamed

    override val stores: Seq[Collection] =
      for (collectionFile <- element.files) yield Parser.parseDo(Collection.parse(
        documentSelector = selectorByName("document").asNamed,
        collectionName = Files.nameAndExtension(Files.pathAndName(collectionFile)._2)._1,
        from = From.url(Files.fileInDirectory(url, collectionFile))))
  }

  private val byCase: ByCase = new ByCase(element.by.get)

  val by: Option[ByCase] = Some(byCase)

  def prettyPrint(): Unit = {
    for (collection <- byCase.stores; document <- collection.documents) {
      // TODO do collection descriptors also!
      // TODO do translations also!
      Util.writeXml(
        Files.url2file(document.url),
        Tei.toXml(document.tei)
      )
    }

    for (named <- nameds.get.by.stores) Util.writeXml(
      Files.url2file(named.url.get),
      Named.toXml(named.copy(id = None))
    )
  }
}

object Store {
  def read(from: From): Store = new Store(
    url = from.url.get,
    element = Parser.parseDo(StoreElement.parse(from))
  )
}
