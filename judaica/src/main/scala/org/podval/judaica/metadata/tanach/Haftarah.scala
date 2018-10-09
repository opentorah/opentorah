package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ProphetSpan
import org.podval.judaica.metadata.{Attributes, LanguageSpec, Metadata, XML}

import scala.xml.Elem

final case class Haftarah(customs: Custom.Of[Seq[ProphetSpan.BookSpan]]) {
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
  ).mapValues { metadata => parse(metadata.attributes, metadata.elements) }

  def parse(attributes: Attributes, elements: Seq[Elem]): Haftarah = {
    val globalSpan = ProphetSpan.parse(attributes)
    attributes.close()

    val (partElements, customElements) = XML.span(elements, "part", "custom")

    val common: Option[Seq[ProphetSpan.BookSpan]] =
      if (partElements.isEmpty && customElements.isEmpty) Some(Seq(globalSpan.resolve)) else
      if (partElements.isEmpty) None else Some(parseParts(partElements, globalSpan))

    // TODO toMap() will silently ignore duplicates...
    val customs: Custom.Sets[Seq[ProphetSpan.BookSpan]] = customElements.map(parseCustom(globalSpan)).toMap

    val result: Custom.Sets[Seq[ProphetSpan.BookSpan]] = common.fold(customs) { common =>
      // TODO updated() will silently ignore duplicates...
      customs.updated(Set[Custom](Custom.Common), common)
    }

    new Haftarah(Custom.denormalize(result))
  }

  private def parseCustom(globalSpan: ProphetSpan.Parsed)(element: Elem): (Set[Custom], Seq[ProphetSpan.BookSpan]) = {
    val (attributes, elements) = XML.open(element, "custom")
    val customs: Set[Custom] = Custom.parse(attributes.doGet("n"))

    val customSpan = ProphetSpan.parse(attributes)
    attributes.close()
    val contextSpan = customSpan.inheritFrom(globalSpan)

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
    require(result.length > 1)
    ProphetSpan.dropNumbers(result)
  }
}
