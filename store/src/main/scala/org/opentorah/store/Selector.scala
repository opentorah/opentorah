package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.{Unparser, Element, Parsable, Parser}

final class Selector(val names: Names) {
  override def toString: String = names.toString

  def bind(store: Store): Binding = Binding(this, store)
}

object Selector extends Element[Selector]("selector") {

  val predefinedSelectors: Seq[Selector] = Seq.empty

  override def contentParsable: Parsable[Selector] = new Parsable[Selector] {
    override val parser: Parser[Selector] = for {
      names <- Names.withDefaultNameParsable()
    } yield new Selector(names)

    override val unparser: Unparser[Selector] = Names.withDefaultNameParsable(_.names)
  }
}
