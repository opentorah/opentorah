package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.{Attribute, Element, Parser}
import scala.xml.Elem

final class Selector(val names: Names) {
  override def toString: String = names.toString

  def bind(store: Store): Binding = Binding(this, store)
}

object Selector extends Element.WithToXml[Selector]("selector") {

  val predefinedSelectors: Seq[Selector] = Seq.empty

  override protected def parser: Parser[Selector] = for {
    names <- Names.withDefaultNameParser
  } yield new Selector(names)

  override protected def attributes(value: Selector): Seq[Attribute.Value[_]] = Seq.empty

  override protected def content(value: Selector): Seq[Elem] =
    Names.toXml(value.names)
}
