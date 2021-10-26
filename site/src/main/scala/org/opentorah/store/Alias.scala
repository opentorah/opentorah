package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class Alias(override val names: Names, val to: String) extends Terminal

object Alias extends Element[Alias]("alias"):
  override def contentParsable: Parsable[Alias] = new Parsable[Alias]:
    private val toAttribute: Attribute.Required[String] = Attribute("to").required

    override def parser: Parser[Alias] = for
      names: Names <- Names.withDefaultNameParsable()
      alias: String <- toAttribute()
    yield Alias(
      names,
      alias
    )

    override def unparser: Unparser[Alias] = Unparser.concat(
      Names.withDefaultNameParsable(_.names),
      toAttribute(_.to)
    )