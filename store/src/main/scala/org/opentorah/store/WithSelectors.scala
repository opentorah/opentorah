package org.opentorah.store

class WithSelectors(inheritedSelectors: Seq[Selector]) {

  protected def definedSelectors: Seq[Selector] = Seq.empty

  final def selectors: Seq[Selector] = inheritedSelectors ++ definedSelectors

  final def selectorByName(name: String): Selector = selectors.find(_.names.hasName(name)).get
}
