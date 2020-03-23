package org.opentorah.store

import java.io.File
import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.util.Files
import org.opentorah.xml.{Element, From, PaigesPrettyPrinter, Parser, Text, ToXml}
import scala.xml.Elem

abstract class By(val store: Store) {
  def selector: Selector.Named

  def stores: Seq[Store]

  final def references(at: Path): Seq[EntityReference] = stores.flatMap { store: Store =>
    store.references(at :+ selector.bind(store))
  }
}

object By {

  class FromElement(
    store: Store,
    element: ByElement
  ) extends By(store) {
    final override def selector: Selector.Named =
      store.selectorByName(element.selector).asNamed

    override def stores: Seq[Store] =
      for (storeElement <- element.stores) yield {
        val (resolvedUrl: URL, resolvedElement: StoreElement.Inline) =
          StoreElement.resolve(store.url, storeElement)

        Store.fromElement(Some(store), resolvedUrl, resolvedElement)
    }

    // TODO use Parsable.parse(from) or Parsable.allMustBe()?
    protected final def filesWithExtensions(url: URL, extension: String): Seq[String] = {
      val directoryName = element.directory.get
      val directory: URL =
        Files.subdirectory(url, directoryName)
      val list: URL =
        Files.fileInDirectory(url, element.list.getOrElse(directoryName + "-list-generated.xml"))

      if (!Files.isFile(directory)) Parser.parseDo(FilesList.parse(From.url(list))) else {
        val result: Seq[String] = Files.filesWithExtensions(Files.url2file(directory), extension).sorted
        if (Files.isFile(list)) writeXml(FilesList.toXml(result), Files.url2file(list))
        result
      }
    }
  }

  def fromElement(
    store: Store,
    element: ByElement
  ): By = new FromElement(store, element)


  object FilesList extends Element[Seq[String]](
    elementName = "filesList",
    parser = Text("file").allMustBe
  ) with ToXml[Seq[String]] {

    override def toXml(value: Seq[String]): Elem =
      <filesList>
        {for (file <- value) yield <file>{file}</file>}
      </filesList>
  }

  // TODO dup!
  def writeXml(
    elem: Elem,
    file: File
  ): Unit = Files.write(
    file,
    content = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" +
      new PaigesPrettyPrinter().render(elem) +
      "\n"
  )
}
