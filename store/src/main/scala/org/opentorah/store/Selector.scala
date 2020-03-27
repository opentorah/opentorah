package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.Element

final class Selector(val names: Names) {
  override def toString: String = names.toString

  def bind(store: Store): Binding = Binding(this, store)
}

object Selector extends Element[Selector]("selector", parser = for {
  names <- Names.withDefaultNameParser
} yield new Selector(names)) {

  val predefinedSelectors: Seq[Selector] = Seq.empty
}
