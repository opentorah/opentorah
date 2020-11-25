package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Title}

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
}

object Store extends StoreComponent {

  def withPath[R](
    path: Path,
    values: Store => Seq[R],
    store: Store
  ): Seq[WithPath[R]] = {
    val fromStore: Seq[WithPath[R]] =
      values(store).map(WithPath[R](path, _))

    val fromEntities: Seq[WithPath[R]] = store.entities.toSeq.flatMap(entities =>
      withPath[R](path :+ entities.selector.bind(entities), values, entities))

    val fromBy: Seq[WithPath[R]] = store.by.toSeq.flatMap(by =>
      by.stores.flatMap(store => withPath[R](path :+ by.selector.bind(store), values, store)))

    fromEntities ++ fromStore ++ fromBy
  }
}
