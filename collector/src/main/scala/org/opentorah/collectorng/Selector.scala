package org.opentorah.collectorng

import org.opentorah.metadata.{Metadata, Names}
import org.opentorah.xml.{Antiparser, Element, Parser}

final class Selector(val names: Names) {
  override def toString: String = names.toString
  def resolves(url: Seq[String]): Boolean = names.hasName(url.head)
  def name: String = names.name
}

object Selector extends Element[Selector]("selector") {

  override val parser: Parser[Selector] = for {
    names <- Names.withDefaultNameParser
  } yield new Selector(names)

  override val antiparser: Antiparser[Selector] = Antiparser(
    content = value => Names.toXml(value.names)
  )

  // TODO ZIOify
  def byName(name: String): Selector = values.find(_.names.hasName(name)).get

  // Note: this is lazy because Selector needs to be initialized when it is passed as a parameter to load:
  private lazy val values: Seq[Selector] = Metadata.loadResource(this, this)
}
