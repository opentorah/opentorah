package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.{Attribute, Parser, Unparser}

trait By[+T <: Store] extends Stores[T]:
  def selector: Selector

  final override def names: Names = selector.names

object By:
  trait WithSelector[+T <: Store](selectorName: String) extends By[T]:
    override def selector: Selector = Selector.getForName(selectorName)

  private val selectorAttribute: Attribute.Required[String] = Attribute("selector").required

  val selectorParser: Parser[String] = selectorAttribute()

  def selectorUnparser[T <: By[?]]: Unparser[T] = selectorAttribute(_.names.name)