package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.xml.{Antiparser, Attribute, Parser}

trait By extends Store {

  def selector: Selector

  final override def names: Names = selector.names
}

object By {
  private val selectorAttribute: Attribute.Required[String] = Attribute("selector").required

  val selector: Parser[Selector] = selectorAttribute().map(Selector.byName)

  def selectorToXml[T <: By]: Antiparser[T] = selectorAttribute(_.selector.name)
}
