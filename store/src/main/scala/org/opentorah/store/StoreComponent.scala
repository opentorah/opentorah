package org.opentorah.store

import java.net.URL
import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Unparser, Attribute, Parser, PrettyPrinter}

class StoreComponent extends Component("store") {

  protected final val fromAttribute: Attribute.Optional[String] = Attribute("from").optional

  final case class InlineClass(
    names: Names,
    from: Option[String],
    title: Option[Title.Value],
    storeAbstract: Option[Abstract.Value],
    body: Option[Body.Value],
    selectors: Seq[Selector],
    entities: Option[Entities.Element],
    by: Option[By.Element],
    className: Option[String]
  ) extends Element with WithClassName

  override type Inline = InlineClass

  override def classOfInline: Class[InlineClass] = classOf[InlineClass]

  override def inlineParser(className: Option[String]): Parser[Inline] = for {
    names <- Names.withDefaultNameParsable()
    from <- fromAttribute()
    title <- Title.element.optional()
    storeAbstract <- Abstract.element.optional()
    body <- Body.element.optional()
    selectors <- Selector.seq()
    entities <- Entities.parsable.optional()
    by <- By.parsable.optional()
  } yield InlineClass(
    names,
    from,
    title,
    storeAbstract,
    body,
    selectors,
    entities,
    by,
    className
  )

  override protected val inlineUnparser: Unparser[Inline] = Unparser.concat(
    Names.withDefaultNameParsable(_.names),
    fromAttribute(_.from),
    Component.typeAttribute(_.className),
    Title.element.optional(_.title),
    Abstract.element.optional(_.storeAbstract),
    Body.element.optional(_.body),
    Selector.seq(_.selectors),
    Entities.parsable.optional(_.entities),
    By.parsable.optional(_.by)
  )

  class FromElement(
    inheritedSelectors: Seq[Selector],
    urls: Urls,
    val element: Inline
  ) extends Store(inheritedSelectors, urls) {

    final override def names: Names =
      element.names

    final override def title: Option[Title.Value] =
      element.title

    final override def storeAbstract: Option[Abstract.Value] =
      element.storeAbstract

    final override def body: Option[Body.Value] =
      element.body

    final override protected def definedSelectors: Seq[Selector] =
      element.selectors

    final override val entities: Option[Entities] =
      element.entities.map(entities => new Entities(selectors, urls.inline, entities))

    override def by: Option[By[Store]] = element.by.map(byElement => By.fromElement[By[Store]](
      selectors,
      urls.inline,
      byElement,
      creator = By.creator
    ))
  }

  def creator: Creator[Store] =
    new FromElement(_, _, _)

  def read(fromUrl: URL): Store =
    read[Store](fromUrl, creator = creator)

  val prettyPrinter: PrettyPrinter = new PrettyPrinter(
    nestElements = Set("p"),
    alwaysStackElements = Set("store", "by")
  )
}
