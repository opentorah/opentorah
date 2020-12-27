package org.opentorah.collectorng

import org.opentorah.metadata.{Metadata, Names, WithNames}
import org.opentorah.xml.{Antiparser, Attribute, Element, Parser}

final class Selector(
  val names: Names,
  val title: Option[String]
) extends WithNames {
  def name: String = names.name
}

object Selector extends Element[Selector]("selector") {

  private val titleAttribute: Attribute[String] = Attribute("title")

  override val parser: Parser[Selector] = for {
    names <- Names.withDefaultNameParser
    title <- titleAttribute.optional
  } yield new Selector(
    names,
    title
  )

  override val antiparser: Antiparser[Selector] = Antiparser.concat(
    Names.antiparser(_.names), // TODO verify that this works!
    titleAttribute.toXmlOption(_.title)
  )

  def byName(name: String): Selector = values.find(_.names.hasName(name)).get

  // Note: this is lazy because Selector needs to be initialized when it is passed as a parameter to load:
  private lazy val values: Seq[Selector] = Metadata.loadResource(this, this)
}
