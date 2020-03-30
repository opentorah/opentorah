package org.opentorah.store

import java.net.URL
import org.opentorah.util.Files
import org.opentorah.xml.{Element, PaigesPrettyPrinter, Parser, Text, ToXml}
import zio.ZIO
import scala.xml.Elem

abstract class By[+S <: Store](
  inheritedSelectors: Seq[Selector],
  baseUrl: URL
) extends WithSelectors(inheritedSelectors) {

  def selector: Selector

  final lazy val stores: Seq[S] = Parser.parseDo(Parser.collectAll(load))

  protected def load: Seq[Parser[S]]

  final def withPath[R](path: Path, values: Store => Seq[R]): Seq[WithPath[R]] =
    stores.flatMap(store => store.withPath[R](path :+ selector.bind(store), values))
}

object By {

  class FromElement[+S <: Store](
    inheritedSelectors: Seq[Selector],
    baseUrl: URL,
    val element: ByElement
  ) extends By[S](inheritedSelectors, baseUrl) {

    final override def selector: Selector = selectorByName(element.selector)

    final override protected def load: Seq[Parser[S]] =
      if (element.directory.isEmpty) {
        for (storeElement <- element.stores) // TODO Parserify:
        yield ZIO.succeed(Store.fromElement[S](selectors, fromUrl = None, baseUrl, storeElement))

      } else {
        val directoryName: String = element.directory.get
        val directory: URL = Files.subdirectory(baseUrl, directoryName)
        val list: URL = Files.fileInDirectory(baseUrl, element.list.getOrElse(directoryName + "-list-generated.xml"))

        val fileNames: Seq[String] = if (!Files.isFile(directory)) Parser.parseDo(By.filesList.parse(list)) else {
          val result: Seq[String] = Files.filesWithExtensions(Files.url2file(directory), "xml").sorted
          if (Files.isFile(list)) new PaigesPrettyPrinter().writeXml(
            file = Files.url2file(list),
            elem = By.filesList.toXml(result)
          )
          result
        }

        def fileInDirectory(fileName: String): URL = Files.fileInDirectory(directory, fileName + ".xml")
        loadFromDirectory(fileNames, fileInDirectory)
      }

    protected def loadFromDirectory(
      fileNames: Seq[String],
      fileInDirectory: String => URL
    ): Seq[Parser[S]] =
      throw new IllegalArgumentException("loadFromDirectory() needs to be overridden")
  }

  private object filesList extends Element[Seq[String]](
    elementName = "filesList",
    parser = Text("file").allMustBe
  ) with ToXml[Seq[String]] {

    override def toXml(value: Seq[String]): Elem =
      <filesList>
        {for (file <- value) yield <file>{file}</file>}
      </filesList>
  }
}
