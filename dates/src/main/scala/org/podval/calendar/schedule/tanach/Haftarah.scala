package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ProphetSpan
import org.podval.judaica.metadata.tanach.{Custom, Parsha, WithNumber}
import org.podval.judaica.metadata.{Attributes, LanguageSpec, Metadata, XML}

import scala.xml.Elem

object Haftarah {
  type Customs = Custom.Of[Seq[ProphetSpan.BookSpan]]

  type OptionalCustoms = Custom.Of[Option[Seq[ProphetSpan.BookSpan]]]

  def toString(customs: OptionalCustoms): String = toString(customs, LanguageSpec.empty)

  def toString(customs: OptionalCustoms, spec: LanguageSpec): String = {
    customs.toSeq.map { case (custom, spansOpt) =>
      custom.toString(spec) + ": " + spansOpt.fold("")(spans => ProphetSpan.toString(spans, spec))
    }.mkString("\n")
  }

  final def forParsha(parsha: Parsha): OptionalCustoms = haftarah(parsha)

  private lazy val haftarah: Map[Parsha, OptionalCustoms] = Metadata.loadMetadata(
    keys = Parsha.values,
    obj = this,
    elementName = "week",
    resourceName = Some("Haftarah")
  ).mapValues { metadata => parse(metadata.attributes, metadata.elements, full = true) }

  def parse(element: Elem, full: Boolean): Haftarah.OptionalCustoms = {
    val (attributes, elements) = XML.open(element, "haftarah")
    Haftarah.parse(attributes, elements, full = full)
  }

  def parse(attributes: Attributes, elements: Seq[Elem], full: Boolean): OptionalCustoms = {
    val span = ProphetSpan.parse(attributes)
    attributes.close()

    val (partElements, customElements) = XML.span(elements, "part", "custom")

    val common: Option[Seq[ProphetSpan.BookSpan]] =
      if (partElements.isEmpty && customElements.isEmpty) Some(Seq(span.resolve)) else
      if (partElements.isEmpty) None else Some(parseParts(partElements, span))

    // TODO toMap() will silently ignore duplicates...
    val customs: Custom.Sets[Option[Seq[ProphetSpan.BookSpan]]] = customElements.map(parseCustom(span)).toMap

    val result: Custom.Sets[Option[Seq[ProphetSpan.BookSpan]]] = common.fold(customs) { common =>
      // TODO updated() will silently ignore duplicates...
      customs.updated(Set[Custom](Custom.Common), Some(common))
    }

    Custom.denormalize(result, full)
  }

  private def parseCustom(ancestorSpan: ProphetSpan.Parsed)(element: Elem): (Set[Custom], Option[Seq[ProphetSpan.BookSpan]]) = {
    val (attributes, elements) = XML.open(element, "custom")
    val customs: Set[Custom] = Custom.parse(attributes.doGet("n"))

    val result: Option[Seq[ProphetSpan.BookSpan]] =
      if (attributes.getBoolean("empty").contains(true)) None else Some {

        val span = ProphetSpan.parse(attributes).inheritFrom(ancestorSpan)
        attributes.close()

        val partElements = XML.span(elements, "part")

        if (partElements.isEmpty) Seq(span.resolve)
        else parseParts(partElements, span)
      }

    customs -> result
  }

  private def parseParts(elements: Seq[Elem], ancestorSpan: ProphetSpan.Parsed): Seq[ProphetSpan.BookSpan] = {
    val result: Seq[ProphetSpan.Numbered] = elements.map(element =>
      ProphetSpan.parseNumbered(XML.openEmpty(element, "part"), ancestorSpan))
    WithNumber.checkConsecutive(result, "part")
    require(result.length > 1)
    ProphetSpan.dropNumbers(result)
  }
}
