package org.opentorah.store

import org.opentorah.metadata.{LanguageSpec, Names}

sealed trait Binding {
  def selectedName(languageSpec: LanguageSpec): String = this match {
    case Binding.Named(_, names) => names.doFind(languageSpec).name
    case Binding.Numbered(_, number) => number.toString
    case Binding.Nullary(selector) => selector.names.name
  }

  def getSelector: Selector = this match {
    case Binding.Named(selector, _) => selector
    case Binding.Numbered(selector, _) => selector
    case Binding.Nullary(selector) => selector
  }
}

object Binding {

  case class Numbered(selector: Selector.Numbered, number: Int) extends Binding

  case class Named(selector: Selector.Named, names: Names) extends  Binding

  case class Nullary(selector: Selector.Nullary) extends Binding
}
