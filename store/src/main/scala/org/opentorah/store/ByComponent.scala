package org.opentorah.store

import java.net.URL
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Parser, PrettyPrinter, Text}
import zio.ZIO
import scala.xml.Elem

class ByComponent extends Component("by") {

  private val selectorAttribute: Attribute[String] = Attribute("selector")
  private val directoryAttribute: Attribute[String] = Attribute("directory")
  private val listAttribute: Attribute[String] = Attribute("list")

  final case class InlineClass(
    selector: String,
    directory: Option[String],
    list: Option[String],
    stores: Seq[Store.Element],
    className: Option[String]
  ) extends Element with WithClassName

  override type Inline = InlineClass

  override def classOfInline: Class[InlineClass] = classOf[InlineClass]

  override def inlineParser(className: Option[String]): Parser[Inline] = for {
    selector <- selectorAttribute.required
    directory <- directoryAttribute.optional
    list <- listAttribute.optional
    stores <- Store.parsable.all
  } yield InlineClass(
    selector,
    directory,
    list,
    stores,
    className
  )

  override protected def inlineAttributes(value: Inline): Seq[Attribute.Value[_]] = Seq(
    selectorAttribute.withValue(value.selector),
    directoryAttribute.withValue(value.directory),
    listAttribute.withValue(value.list),
    Component.typeAttribute.withValue(value.className)
  )

  override protected def inlineContent(value: Inline): Seq[Elem] =
    Store.parsable.toXml(value.stores)

  abstract class FromElement[+S <: Store](
    inheritedSelectors: Seq[Selector],
    urls: Urls,
    val element: Inline
  ) extends By[S](inheritedSelectors, urls) {

    final override def selector: Selector = selectorByName(element.selector)

    protected def storeCreator: Store.Creator[S]

    final override protected def load: Seq[Parser[S]] =
      if (element.directory.isEmpty) {
        for (storeElement <- element.stores) yield ZIO.succeed(Store.fromElement[S](
          selectors,
          urls.inline,
          element = storeElement,
          creator = storeCreator
        ))

      } else {
        val directoryName: String = element.directory.get
        val directory: URL = Files.subdirectory(urls.baseUrl, directoryName)
        val list: URL = Files.fileInDirectory(urls.baseUrl, element.list.getOrElse(directoryName + "-list-generated.xml"))

        val fileNames: Seq[String] = if (!Files.isFile(directory)) Parser.parseDo(filesList.parse(list)) else {
          val result: Seq[String] = Files.filesWithExtensions(Files.url2file(directory), "xml").sorted
          if (Files.isFile(list)) Files.write(
            file = Files.url2file(list),
            content = PrettyPrinter.default.renderXml(filesList.toXml(result))
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

  def creator: Creator[By[Store]] =
    new FromElement[Store](_, _, _) {
      override protected def storeCreator: Store.Creator[Store] = Store.creator
    }

  private object filesList extends org.opentorah.xml.Element.WithToXml[Seq[String]]("filesList") {

    override protected def parser: Parser[Seq[String]] = Text("file").all

    override protected def attributes(value: Seq[String]): Seq[Attribute.Value[_]] = Seq.empty

    override protected def content(value: Seq[String]): Seq[Elem] =
      for (file <- value) yield <file>{file}</file>
  }
}
