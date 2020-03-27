package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.{Attribute, Element}

sealed abstract class Selector(
  val names: Names
) {
  override def toString: String = names.toString

  def asNamed: Selector.Named
  def asNumbered: Selector.Numbered
  def asNullary: Selector.Nullary
}

object Selector extends Element[Selector]("selector", parser = for {
  selectorType <- Attribute("type").optional
  names <- Names.withDefaultNameParser
} yield Selector.make(selectorType, names)) {

  final class Numbered(names: Names) extends Selector(names) {
    def asNamed: Named = throw new IllegalArgumentException
    def asNumbered: Numbered = this
    def asNullary: Nullary = throw new IllegalArgumentException

    def bind(number: Int): Binding.Numbered = Binding.Numbered(this, number)
  }

  final class Named(names: Names) extends Selector(names) {
    def asNamed: Named = this
    def asNumbered: Numbered = throw new IllegalArgumentException
    def asNullary: Nullary = throw new IllegalArgumentException

    def bind(store: Store): Binding.Named = Binding.Named(this, store)
  }

  final class Nullary(names: Names) extends Selector(names) {
    def asNamed: Named = throw new IllegalArgumentException
    def asNumbered: Numbered = throw new IllegalArgumentException
    def asNullary: Nullary = this

    def bind: Binding.Nullary = Binding.Nullary(this)
  }

  private def make(selectorType: Option[String], names: Names): Selector = selectorType.getOrElse("named") match {
    case "named" => new Named(names)
    case "numbered" => new Numbered(names)
    case "nullary" => new Nullary(names)
  }

  val predefinedSelectors: Seq[Selector] = Seq.empty
}
