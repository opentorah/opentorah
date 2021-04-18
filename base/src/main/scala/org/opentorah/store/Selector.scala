package org.opentorah.store

import org.opentorah.metadata.{Metadata, Names, WithNames}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class Selector(
  val names: Names,
  val title: Option[String]
) extends WithNames {
  def name: String = names.name
}

object Selector extends Element[Selector]("selector") {

  private val titleAttribute: Attribute.Optional[String] = Attribute("title").optional

  override def contentParsable: Parsable[Selector] = new Parsable[Selector] {
    override val parser: Parser[Selector] = for {
      names <- Names.withDefaultNameParsable()
      title <- titleAttribute()
    } yield new Selector(
      names,
      title
    )

    override val unparser: Unparser[Selector] = Unparser.concat(
      Names.withDefaultNameParsable(_.names),
      titleAttribute(_.title)
    )
  }

  def byName(name: String): Selector = values.find(_.names.hasName(name)).get

  // Note: this is lazy because Selector needs to be initialized when it is passed as a parameter to load:
  private lazy val values: Seq[Selector] = Parser.run(Metadata.loadResource(this, this))
}
