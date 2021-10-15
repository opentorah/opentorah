package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.{Attribute, Parser, Unparser}

trait By[+T <: Store] extends Store.NonTerminal[T]:

  def selector: Selector

  final override def names: Names = selector.names

object By:
  trait WithSelector[+T <: Store](selectorName: String) extends By[T]:
    override def selector: Selector = Selector.getForName(selectorName)

  class Pure[+T <: Store](selectorName: String, override val storesPure: Seq[T])
    extends WithSelector[T](selectorName), Stores.Pure[T]

  abstract class Numbered[T <: Store.Numbered](selectorName: String)
    extends WithSelector[T](selectorName), Stores.Numbered[T]

  private val selectorAttribute: Attribute.Required[String] = Attribute("selector").required

  val selectorParser: Parser[String] = selectorAttribute()

  def selectorUnparser[T <: By[_]]: Unparser[T] = selectorAttribute(_.names.name)
