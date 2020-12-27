package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.xml.{Antiparser, Attribute, Parser}

trait By extends Store {

  def selector: Selector

  final override def names: Names = selector.names
}

object By {
  private val selectorAttribute: Attribute[String] = Attribute("selector")

  val selector: Parser[Selector] = selectorAttribute.required.map(Selector.byName)

  def selectorToXml[T <: By]: Antiparser[T] = selectorAttribute.toXml(_.selector.name)
}
