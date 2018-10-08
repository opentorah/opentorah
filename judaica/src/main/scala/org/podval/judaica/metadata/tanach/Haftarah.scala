package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, LanguageSpec, Metadata, XML}

import scala.xml.Elem

final class Haftarah(val customs: Custom.Of[Seq[ProphetSpan.BookSpan]]) {
  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String = {
    customs.toSeq.map { case (custom, spans) =>
      custom.toString(spec) + ": " + ProphetSpan.toString(spans, spec)
    }.mkString("\n")
  }
}

object Haftarah {
  lazy val haftarah: Map[Parsha, Haftarah] = Metadata.loadMetadata(
    keys = Parsha.values,
    obj = Haftarah.this,
    elementName = "week"
  ).mapValues { metadata => new Haftarah(parse(metadata.attributes, metadata.elements)) }

  def parse(attributes: Attributes, elements: Seq[Elem]): Custom.Of[Seq[ProphetSpan.BookSpan]] = {
    val globalSpan = ProphetSpan.parse(attributes)
    attributes.close()

    // TODO allow 'part' elements before 'custom' elements; treat them as 'Common'; clean up SpecialDay.xml.
    val customElements = XML.span(elements, "custom")

    val result: Map[Set[Custom], Seq[ProphetSpan.BookSpan]] =
      if (customElements.isEmpty) Map(Set[Custom](Custom.Common) -> Seq(globalSpan.resolve))
      else customElements.map(element => parseCustom(element, globalSpan)).toMap

    Custom.denormalize(result)
  }

  private def parseCustom(element: Elem, weekSpan: ProphetSpan.Parsed): (Set[Custom], Seq[ProphetSpan.BookSpan]) = {
    val (attributes, elements) = XML.open(element, "custom")
    val customs: Set[Custom] = Custom.parse(attributes.doGet("n"))

    val customSpan = ProphetSpan.parse(attributes)
    attributes.close()
    val contextSpan = customSpan.inheritFrom(weekSpan)

    val partElements = XML.span(elements, "part")

    val result: Seq[ProphetSpan.BookSpan] =
      if (partElements.isEmpty) Seq(contextSpan.resolve)
      else parseParts(partElements, contextSpan)

    customs -> result
  }

  private def parseParts(elements: Seq[Elem], contextSpan: ProphetSpan.Parsed): Seq[ProphetSpan.BookSpan] = {
    val result: Seq[ProphetSpan.Numbered] = elements.map(element =>
      ProphetSpan.parseNumbered(XML.openEmpty(element, "part"), contextSpan))
    WithNumber.checkConsecutive(result, "part")
    result.map(_.span)
  }
}
