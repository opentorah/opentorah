package org.opentorah.store

import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.Names

// TODO make the base URL transient;
// factor the URL the file was read from into a FromUrl trait;
// pretty-print everything that was read from a file.
abstract class Store(
  inheritedSelectors: Seq[Selector],
  val url: URL
) extends WithSelectors(inheritedSelectors) {
  def names: Names

  def entities: Option[Entities] = None

  def by: Option[By] = None

  def title: Option[StoreElement.Title.Value] = None

  def storeAbstract: Option[StoreElement.Abstract.Value] = None

  def notes: StoreElement.Notes = new StoreElement.Notes(Seq.empty)

  def references: Seq[EntityReference] = Seq.empty

  // TODO fish references out of the title, abstract and notes too!
  final def withPath[R](
    path: Path = Path.empty,
    values: Store => Seq[R]
  ): Seq[WithPath[R]] = {
    val fromStore: Seq[WithPath[R]] =
      values(this).map(WithPath[R](path, _))

    val fromEntities: Seq[WithPath[R]] =
      entities.toSeq.flatMap(entities => entities.by.withPath[R](path :+ entities.selector.bind, values))

    val fromBy: Seq[WithPath[R]] =
      by.toSeq.flatMap(_.withPath[R](path, values))

    fromEntities ++ fromStore ++ fromBy
  }
}

object Store {

  class FromElement(
    inheritedSelectors: Seq[Selector],
    url: URL,
    element: StoreElement.Inline
  ) extends Store(inheritedSelectors, url) {

    final override def names: Names =
      element.names

    final override protected def definedSelectors: Seq[Selector] =
      element.selectors

    final override val entities: Option[Entities] =
      element.entities.map(entities => new Entities(selectors, url, entities))

    final override def title: Option[StoreElement.Title.Value] =
      element.title

    final override def storeAbstract: Option[StoreElement.Abstract.Value] =
      element.storeAbstract

    final override def notes: StoreElement.Notes =
      element.notes

    override def by: Option[By] =
      element.by.map(byElement => By.fromElement(selectors, url, byElement))
  }

  def fromElement(
    inheritedSelectors: Seq[Selector],
    url: URL,
    element: StoreElement.Inline
  ): Store = {
    if (element.storeType.isDefined) Class.forName(element.storeType.get)
      .getConstructor(classOf[Seq[Selector]], classOf[URL], classOf[StoreElement.Inline])
      .newInstance(inheritedSelectors, url, element)
      .asInstanceOf[Store]
    else new FromElement(
      inheritedSelectors,
      url,
      element
    )
  }

  def fromUrl(url: URL): Store = {
    val (storeUrl, storeElement) = StoreElement.resolve(url, StoreElement.read(url))
    fromElement(
      inheritedSelectors = Selector.predefinedSelectors,
      storeUrl,
      storeElement
    )
  }
}
