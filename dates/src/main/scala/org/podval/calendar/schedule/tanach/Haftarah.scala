package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ProphetSpan
import org.podval.judaica.metadata.tanach.{Custom, Parsha, WithNumber}
import org.podval.judaica.metadata.{Attributes, Metadata, XML}

import scala.xml.Elem

object Haftarah {
  type Haftarah = Seq[ProphetSpan.BookSpan]

  type Customs = Custom.Of[Haftarah]

  final def forParsha(parsha: Parsha): Customs = haftarah(parsha)

  private lazy val haftarah: Map[Parsha, Customs] = Metadata.loadMetadata(
    keys = Parsha.values,
    obj = this,
    elementName = "week",
    resourceName = Some("Haftarah")
  ).mapValues { metadata => parse(metadata.attributes, metadata.elements, full = true) }

  def parse(element: Elem, full: Boolean): Haftarah.Customs = {
    val (attributes, elements) = XML.open(element, "haftarah")
    parse(attributes, elements, full = full)
  }

  private def parse(attributes: Attributes, elements: Seq[Elem], full: Boolean): Customs = {
    val span = ProphetSpan.parse(attributes)
    attributes.close()

    val (partElements, customElements) = XML.span(elements, "part", "custom")

    val common: Option[Seq[ProphetSpan.BookSpan]] =
      if (partElements.isEmpty && customElements.isEmpty) Some(Seq(span.resolve)) else
      if (partElements.isEmpty) None else Some(parseParts(partElements, span))

    // TODO toMap() will silently ignore duplicates...
    val customs: Custom.Sets[Seq[ProphetSpan.BookSpan]] = customElements.map(parseCustom(span)).toMap

    val result: Custom.Sets[Seq[ProphetSpan.BookSpan]] = common.fold(customs) { common =>
      // TODO updated() will silently ignore duplicates...
      customs.updated(Set[Custom](Custom.Common), common)
    }

    Custom.Of(result, full)
  }

  private def parseCustom(ancestorSpan: ProphetSpan.Parsed)(element: Elem): (Set[Custom], Seq[ProphetSpan.BookSpan]) = {
    val (customs, attributes, elements) = open(element)
    customs -> parseCustom(ancestorSpan, attributes, elements)
  }

  private def open(element: Elem): (Set[Custom], Attributes, Seq[Elem]) = {
    val (attributes, elements) = XML.open(element, "custom")
    val customs: Set[Custom] = Custom.parse(attributes.doGet("n"))
    (customs, attributes, elements)
  }

  private def parseCustom(
    ancestorSpan: ProphetSpan.Parsed,
    attributes: Attributes,
    elements: Seq[Elem]
  ): Seq[ProphetSpan.BookSpan] = {
    val span = ProphetSpan.parse(attributes).inheritFrom(ancestorSpan)
    attributes.close()

    val partElements = XML.span(elements, "part")

    if (partElements.isEmpty) Seq(span.resolve)
    else parseParts(partElements, span)
  }

  private def parseParts(elements: Seq[Elem], ancestorSpan: ProphetSpan.Parsed): Seq[ProphetSpan.BookSpan] = {
    val result: Seq[ProphetSpan.Numbered] = elements.map(element =>
      ProphetSpan.parseNumbered(XML.openEmpty(element, "part"), ancestorSpan))
    WithNumber.checkConsecutive(result, "part")
    require(result.length > 1)
    ProphetSpan.dropNumbers(result)
  }
}
