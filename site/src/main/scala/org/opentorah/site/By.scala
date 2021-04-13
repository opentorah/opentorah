package org.opentorah.site

import org.opentorah.metadata.Names
import org.opentorah.xml.{Unparser, Attribute, Parser}

trait By extends Store {

  def selector: Selector

  final override def names: Names = selector.names
}

object By {
  private val selectorAttribute: Attribute.Required[String] = Attribute("selector").required

  val selectorParser: Parser[Selector] = selectorAttribute().map(Selector.byName)

  def selectorUnparser[T <: By]: Unparser[T] = selectorAttribute(_.selector.name)
}
