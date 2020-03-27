package org.opentorah.store

import java.net.URL
import org.opentorah.util.Files
import org.opentorah.xml.{Element, From, PaigesPrettyPrinter, Parser, Text, ToXml}
import scala.xml.Elem

// TODO it seems that I need to add type parameter to the By before I can:
// - add caching to the By.stores()/load();
// - turn Entities into Store (with two Bys);
// - get rid of the Nullary selectors and remove Selector.type;
abstract class By(inheritedSelectors: Seq[Selector]) extends WithSelectors(inheritedSelectors) {
  def selector: Selector.Named

  def stores: Seq[Store]

  final def withPath[R](path: Path, values: Store => Seq[R]): Seq[WithPath[R]] =
    stores.flatMap(store => store.withPath[R](path :+ selector.bind(store), values))
}

object By {

  class FromElement(
    inheritedSelectors: Seq[Selector],
    url: URL,
    element: ByElement
  ) extends By(inheritedSelectors) {
    final override def selector: Selector.Named =
      selectorByName(element.selector).asNamed

    override def stores: Seq[Store] =
      for (storeElement <- element.stores) yield {
        val (resolvedUrl: URL, resolvedElement: StoreElement.Inline) =
          StoreElement.resolve(url, storeElement)

        Store.fromElement(selectors, resolvedUrl, resolvedElement)
    }

    // TODO allow for inline parsing with Parsable.allMustBe() and from-file parsing with Parsable.parse(from)?
    protected final def filesWithExtensions(url: URL, extension: String): Seq[String] = {
      val directoryName = element.directory.get
      val directory: URL =
        Files.subdirectory(url, directoryName)
      val list: URL =
        Files.fileInDirectory(url, element.list.getOrElse(directoryName + "-list-generated.xml"))

      if (!Files.isFile(directory)) Parser.parseDo(FilesList.parse(From.url(list))) else {
        val result: Seq[String] = Files.filesWithExtensions(Files.url2file(directory), extension).sorted
        if (Files.isFile(list))
          new PaigesPrettyPrinter().writeXml(Files.url2file(list), FilesList.toXml(result))
        result
      }
    }
  }

  def fromElement(
    inheritedSelectors: Seq[Selector],
    url: URL,
    element: ByElement
  ): By = new FromElement(
    inheritedSelectors,
    url,
    element
  )

  object FilesList extends Element[Seq[String]](
    elementName = "filesList",
    parser = Text("file").allMustBe
  ) with ToXml[Seq[String]] {

    override def toXml(value: Seq[String]): Elem =
      <filesList>
        {for (file <- value) yield <file>{file}</file>}
      </filesList>
  }
}
