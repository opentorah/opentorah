package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.{Antiparser, Element, Parser}

final class Selector(val names: Names) {
  override def toString: String = names.toString

  def bind(store: Store): Binding = Binding(this, store)
}

object Selector extends Element.WithToXml[Selector]("selector") {

  val predefinedSelectors: Seq[Selector] = Seq.empty

  override val parser: Parser[Selector] = for {
    names <- Names.withDefaultNameParser
  } yield new Selector(names)

  override protected val antiparser: Antiparser[Selector] = Antiparser(
    content = value => Names.toXml(value.names)
  )
}
