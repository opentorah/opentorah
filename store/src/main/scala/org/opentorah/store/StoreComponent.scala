package org.opentorah.store

import java.net.URL
import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Attribute, Parser, PrettyPrinter}
import scala.xml.Node

class StoreComponent extends Component("store") {

  protected final val fromAttribute: Attribute[String] = Attribute("from")

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
    names <- Names.withDefaultNameParser
    from <- fromAttribute.optional
    title <- Title.parsable.optional
    storeAbstract <- Abstract.parsable.optional
    body <- Body.parsable.optional
    selectors <- Selector.all
    entities <- Entities.parsable.optional
    by <- By.parsable.optional
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

  override protected def inlineAttributes(value: Inline): Seq[Attribute.Value[_]] =
    Names.antiparser.attributes(value.names) ++ Seq(
      fromAttribute.withValue(value.from),
      Component.typeAttribute.withValue(value.className)
    )

  override protected def inlineContent(value: Inline): Seq[Node] =
    Names.antiparser.premap[Inline](_.names).content(value) ++
    Title.parsable.elementAntiparserOption.premap[Inline](_.title).content(value) ++
    Abstract.parsable.elementAntiparserOption.premap[Inline](_.storeAbstract).content(value) ++
    Body.parsable.elementAntiparserOption.premap[Inline](_.body).content(value) ++
    Selector.elementAntiparserSeq.premap[Inline](_.selectors).content(value) ++
    Entities.parsable.elementAntiparserOption.premap[Inline](_.entities).content(value) ++
    By.parsable.elementAntiparserOption.premap[Inline](_.by).content(value)

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
