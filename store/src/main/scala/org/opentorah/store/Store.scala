package org.opentorah.store

import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Attribute, Parser}
import scala.xml.{Elem, Node}

abstract class Store(
  inheritedSelectors: Seq[Selector],
  urls: Urls
) extends ComponentBase(inheritedSelectors, urls) {

  def names: Names

  override def toString: String = names.name

  def title: Option[Title.Value] = None

  def storeAbstract: Option[Abstract.Value] = None

  def body: Option[Body.Value] = None

  def entities: Option[Entities] = None

  def by: Option[By[Store]] = None

  def references: Seq[EntityReference]

  final def withPath[R](
    path: Path = Path.empty,
    values: Store => Seq[R]
  ): Seq[WithPath[R]] = {
    val fromStore: Seq[WithPath[R]] =
      values(this).map(WithPath[R](path, _))

    val fromEntities: Seq[WithPath[R]] = entities.toSeq.flatMap(entities =>
      entities.withPath[R](path :+ entities.selector.bind(entities), values))

    val fromBy: Seq[WithPath[R]] =
      by.toSeq.flatMap(_.withPath[R](path, values))

    fromEntities ++ fromStore ++ fromBy
  }
}

object Store extends Component("store") {

  private val nAttribute: Attribute[String] = Attribute("n")
  private val fromAttribute: Attribute[String] = Attribute("from")

  final case class Inline(
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

  override def classOfInline: Class[Inline] = classOf[Inline]

  override def inlineParser(className: Option[String]): Parser[Inline] = for {
    names <- Names.withDefaultNameParser
    from <- fromAttribute.optional
    title <- Title.parsable.optional
    storeAbstract <- Abstract.parsable.optional
    body <- Body.parsable.optional
    selectors <- Selector.all
    entities <- Entities.parsable.optional
    by <- By.parsable.optional
  } yield Inline(
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

  override protected def inlineAttributes(value: Inline): Seq[Attribute.Value[_]] = Seq(
    nAttribute.withValue(value.names.getDefaultName),
    fromAttribute.withValue(value.from),
    Component.typeAttribute.withValue(value.className)
  )

  override protected def inlineContent(value: Inline): Seq[Elem] =
    (if (value.names.getDefaultName.isDefined) Seq.empty else Names.toXml(value.names)) ++
    Title.parsable.toXml(value.title) ++
    Abstract.parsable.toXml(value.storeAbstract) ++
    Body.parsable.toXml(value.body) ++
    Selector.toXml(value.selectors) ++
    Entities.parsable.toXml(value.entities) ++
    By.parsable.toXml(value.by)

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

    final override def references: Seq[EntityReference] = {
      val lookInto: Seq[Node] =
        title.map(_.xml).getOrElse(Seq.empty) ++
        storeAbstract.map(_.xml).getOrElse(Seq.empty) ++
        body.map(_.xml).getOrElse(Seq.empty)

      EntityReference.from(lookInto)
    }
  }

  def creator: Creator[Store] =
    new FromElement(_, _, _)

  def read(fromUrl: URL): Store =
    read[Store](fromUrl, creator = Store.creator)
}
