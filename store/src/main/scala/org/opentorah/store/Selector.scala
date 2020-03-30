package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.{Element, ToXml}
import scala.xml.Elem

final class Selector(val names: Names) {
  override def toString: String = names.toString

  def bind(store: Store): Binding = Binding(this, store)
}

object Selector extends Element[Selector]("selector", parser = for {
  names <- Names.withDefaultNameParser
} yield new Selector(names)) with ToXml[Selector] {

  val predefinedSelectors: Seq[Selector] = Seq.empty

  override def toXml(value: Selector): Elem =
    <selector>{Names.toXml(value.names)}</selector>
}
