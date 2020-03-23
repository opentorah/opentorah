package org.opentorah.store

import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.Names

abstract class Store(
  val parent: Option[Store],
  val url: URL
) {
  def names: Names

  def selectors: Seq[Selector] = Seq.empty

  def entities: Option[Entities] = None

  def by: Option[By] = None

  def title: Option[StoreElement.Title.Value] = None

  def storeAbstract: Option[StoreElement.Abstract.Value] = None

  def notes: StoreElement.Notes = new StoreElement.Notes(Seq.empty)

  final def selectorByName(name: String): Selector = {
    val result: Option[Selector] = selectors.find(_.names.hasName(name))
    parent.fold(result.get)(parent => result.getOrElse(parent.selectorByName(name)))
  }

  // TODO fish references out of the collection descriptors too!
  // TODO overridden in Entity
  def references(at: Path): Seq[EntityReference] = {
    val fromEntities: Seq[EntityReference] = entities.toSeq.flatMap(_.references(at))
    val fromBy: Seq[EntityReference] = by.toSeq.flatMap(_.references(at))
    (fromEntities ++ fromBy).filterNot(_.name == scala.xml.Text("?")) // TODO get rid of the filter
  }
}

object Store {

  class FromElement(
    parent: Option[Store],
    url: URL,
    element: StoreElement.Inline
  ) extends Store(parent, url) {

    final override def names: Names =
      element.names

    final override def selectors: Seq[Selector] =
      element.selectors

    final override val entities: Option[Entities] =
      element.entities.map(entities => new Entities(store = this, url, entities))

    final override def title: Option[StoreElement.Title.Value] =
      element.title

    final override def storeAbstract: Option[StoreElement.Abstract.Value] =
      element.storeAbstract

    final override def notes: StoreElement.Notes =
      element.notes

    // TODO lazy-load; turn this into a protected one; in the future - caching...
    override def by: Option[By] =
      element.by.map(byElement => By.fromElement(this, byElement))
  }

  def fromElement(
    parent: Option[Store],
    url: URL,
    element: StoreElement.Inline
  ): Store = {
    if (element.storeType.isDefined) {
      Class.forName(element.storeType.get)
        .getConstructor(classOf[Option[Store]], classOf[URL], classOf[StoreElement.Inline])
        .newInstance(parent, url, element)
        .asInstanceOf[Store]
    } else new FromElement(
      parent,
      url,
      element
    )
  }

  def fromUrl(url: URL): Store = {
    val (storeUrl, storeElement) = StoreElement.resolve(url, StoreElement.read(url))
    fromElement(
      parent = None,
      storeUrl,
      storeElement
    )
  }
}
