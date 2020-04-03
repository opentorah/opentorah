package org.opentorah.store

class ComponentBase(
  inheritedSelectors: Seq[Selector],
  final val urls: Urls
) {
  protected def definedSelectors: Seq[Selector] = Seq.empty

  final def selectors: Seq[Selector] = inheritedSelectors ++ definedSelectors

  final def selectorByName(name: String): Selector = selectors.find(_.names.hasName(name)).get
}
