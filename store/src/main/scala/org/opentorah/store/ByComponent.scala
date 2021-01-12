package org.opentorah.store

import java.net.URL
import org.opentorah.xml.{Unparser, Attribute, Parser}
import zio.ZIO

class ByComponent extends Component("by") {

  private val selectorAttribute: Attribute.Required[String] = Attribute("selector").required
  private val directoryAttribute: Attribute.Optional[String] = Attribute("directory").optional
  private val listAttribute: Attribute.Optional[String] = Attribute("list").optional

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
    selector <- selectorAttribute()
    directory <- directoryAttribute()
    list <- listAttribute()
    stores <- Store.parsable.seq()
  } yield InlineClass(
    selector,
    directory,
    list,
    stores,
    className
  )

  override protected def inlineUnparser: Unparser[Inline] = Unparser.concat(
    selectorAttribute(_.selector),
    directoryAttribute(_.directory),
    listAttribute(_.list),
    Component.typeAttribute(_.className),
    Store.parsable.seq(_.stores)
  )

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
        val (fileNames: Seq[String], fileInDirectory: (String => URL)) = FilesList.get(
          baseUrl = urls.baseUrl,
          directoryName = element.directory.get,
          listName = element.list,
          extension = "xml"
        )

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
}
