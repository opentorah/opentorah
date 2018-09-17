package org.podval.calendar.generate.tanach

import org.podval.calendar.generate.tanach.SpanParser.{NachSpanParsed, NumberedNachSpan, parseNachSpan, parseNumberedNachSpan}
import org.podval.calendar.metadata.MetadataParser.MetadataPreparsed
import org.podval.calendar.metadata.{Language, LanguageSpec, MetadataLoader, XML}
import Custom.Custom
import Parsha.Parsha

import scala.xml.Elem

final class Haftarah(val customs: Custom.Of[Seq[ProphetSpan]]) {
  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String = {
    customs.toSeq.map { case (custom, spans) =>
      custom.toString(spec) + ": " + ProphetSpan.toString(spans, spec)
    }.mkString("\n")
  }
}

object Haftarah extends MetadataLoader {
  override type Key = Parsha.Parsha

  override val values: Seq[Parsha.Parsha] = Parsha.values

  override type Metadata = Haftarah

  def forParsha(parsha: Parsha): Haftarah = toMetadata(parsha)

  override protected def resourceName: String = "Haftarah"

  override protected def elementName: String = "week"

  override protected def parseMetadata(parhsa: Parsha.Parsha,  metadata: MetadataPreparsed): Haftarah = {
    val n = metadata.attributes.doGet("n")
    val parsha = Parsha.forName(n)
    require(parsha.isDefined, s"Unknown Parsha: $n")

    val weekSpan = parseNachSpan(metadata.attributes)
    metadata.attributes.close()

    val customElements = XML.span(metadata.elements, "custom")

    val result: Map[Set[Custom], Seq[ProphetSpan]] =
      if (customElements.isEmpty) Map(Set[Custom](Custom.Common) -> Seq(weekSpan.resolve))
      else customElements.map(element => parseCustom(element, weekSpan)).toMap

    // Check that custom sets do not overlap.
    val customSets: Set[Set[Custom]] = result.keySet

    // TODO check that customs are different.

    new Haftarah(
      customs = result.flatMap { case (customs, spans) => customs.map(custom => custom -> spans) }
    )
  }

  private def parseCustom(element: Elem, weekSpan: NachSpanParsed): (Set[Custom], Seq[ProphetSpan]) = {
    val (attributes, elements) = XML.open(element, "custom")
    val customs: Set[Custom] = Custom.parse(attributes.doGet("n"))

    val customSpan = parseNachSpan(attributes)
    attributes.close()

    val partElements = XML.span(elements, "part")

    val contextSpan = customSpan.inheritFrom(weekSpan)

    val result: Seq[ProphetSpan] =
      if (partElements.isEmpty) Seq(contextSpan.resolve)
      else parseParts(partElements, contextSpan)

    customs -> result
  }

  private def parseParts(elements: Seq[Elem], contextSpan: NachSpanParsed): Seq[ProphetSpan] = {
    val result: Seq[NumberedNachSpan] = elements.map(element =>
      parseNumberedNachSpan(XML.openEmpty(element, "part"), contextSpan))
    // TODO lift the check that the numbers are consecutive from here and everywhere to SpanParser
    require(result.map(_.n) == (1 to result.length))
    result.map(_.span)
  }

  private def printHaftarahList(custom: Custom, spec: LanguageSpec, full: Boolean): Unit = {
    println(custom.toString(spec))
    for (parsha <- Parsha.values) {
      val haftarah: Haftarah = Haftarah.forParsha(parsha)
      val customEffective: Custom = Custom.find(haftarah.customs, custom)
      val spans: Seq[ProphetSpan] = haftarah.customs(customEffective)
      val result: String = ProphetSpan.toString(spans, spec)

      if (customEffective == custom) {
        println(parsha.toString(spec) + ": " + result)
      } else if (full) {
        println(parsha.toString(spec) + " [" + customEffective.toString(spec)  + "]" + ": " + result)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    def printSpans(spans: Seq[Span]): Unit = spans.zipWithIndex.foreach { case (span, index) =>
      println(s"${index+1}: $span")
    }

    //    printSpans(Parsha.Mattos.metadata.daysCombined(Custom.Ashkenaz))

//    Custom.values.foreach { custom => if (custom != Custom.Common) {
//      printHaftarahList(custom, LanguageSpec(Language.Hebrew), full = false)
//      println()
//      println()
//    }}

    printHaftarahList(Custom.Shami, LanguageSpec(Language.Hebrew), full = false)
  }
}
