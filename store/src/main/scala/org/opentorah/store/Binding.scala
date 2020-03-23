package org.opentorah.store

sealed trait Binding {

  def getSelector: Selector = this match {
    case Binding.Named(selector, _) => selector
    case Binding.Numbered(selector, _) => selector
    case Binding.Nullary(selector) => selector
  }

  def getStore: Option[Store] = this match {
    case Binding.Named(_, store) => Some(store)
    case _ => None
  }
}

object Binding {

  case class Numbered(selector: Selector.Numbered, number: Int) extends Binding

  case class Named(selector: Selector.Named, store: Store) extends  Binding

  case class Nullary(selector: Selector.Nullary) extends Binding
}
