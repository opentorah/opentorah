package org.podval.calendar.generate.tanach

import org.podval.calendar.generate.tanach.SpanParser.{NachSpanParsed, parseNachSpan,
  NumberedNachSpan, parseNumberedNachSpan}
import org.podval.calendar.metadata.{MetadataParser, XML}
import org.podval.calendar.metadata.MetadataParser.MetadataPreparsed

import scala.xml.Elem

object HaftarahMetadataParser {

  def parse: Map[Parsha, Haftarah] = {
    val weeks: Seq[MetadataPreparsed] =
      MetadataParser.loadMetadata(this, "Haftarah", "week")
    weeks.map(parseWeek).map(haftarah => haftarah.parsha -> haftarah).toMap
  }

  def parseWeek(metadata: MetadataPreparsed): Haftarah = {
    val n = metadata.attributes.doGet("n")
    val parsha = Parsha.forName(n)
    require(parsha.isDefined, s"Unknown Parsha: $n")

    val weekSpan = parseNachSpan(metadata.attributes)
    metadata.attributes.close()

    val (customElements, tail) = XML.span(metadata.elements, "custom")
    XML.checkNoMoreElements(tail)

    val result: Map[Set[Custom], Seq[NachSpan]] =
      if (customElements.isEmpty) Map(Set[Custom](Custom.Common) -> Seq(weekSpan.resolve))
      else customElements.map(element => parseCustom(element, weekSpan)).toMap

    // Check that custom sets do not overlap.
    val customSets: Set[Set[Custom]] = result.keySet

    // TODO check that customs are different.

    new Haftarah(
      parsha = parsha.get,
      customs = result.flatMap { case (customs, spans) => customs.map(custom => custom -> spans) }
    )
  }

  private def parseCustom(element: Elem, weekSpan: NachSpanParsed): (Set[Custom], Seq[NachSpan]) = {
    val (attributes, elements) = XML.open(element, "custom")
    val customs: Set[Custom] = CustomParser.parse(attributes.doGet("n"))

    val customSpan = parseNachSpan(attributes)
    attributes.close()

    val (partElements, tail) = XML.span(elements, "part")
    XML.checkNoMoreElements(tail)

    val contextSpan = customSpan.inheritFrom(weekSpan)

    val result: Seq[NachSpan] =
      if (partElements.isEmpty) Seq(contextSpan.resolve)
      else parseParts(partElements, contextSpan)

    customs -> result
  }

  private def parseParts(elements: Seq[Elem], contextSpan: NachSpanParsed): Seq[NachSpan] = {
    val result: Seq[NumberedNachSpan] = elements.map(element =>
      parseNumberedNachSpan(XML.openEmpty(element, "part"), contextSpan))
    // TODO lift the check that the numbers are consecutive from here and everywhere to SpanParser
    require(result.map(_.n) == (1 to result.length))
    result.map(_.span)
  }
}
