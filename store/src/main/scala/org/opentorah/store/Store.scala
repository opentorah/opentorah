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

object Store extends StoreComponent