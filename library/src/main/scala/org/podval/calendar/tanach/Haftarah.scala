package org.podval.calendar.tanach

import org.podval.judaica.tanach.{Custom, Parsha, Tanach, WithBookSpans, WithNumber}
import org.podval.judaica.metadata.{Attributes, LanguageSpec, Metadata, XML}
import org.podval.judaica.util.Util

import scala.xml.Elem

final case class Haftarah private(override val spans: Seq[Haftarah.BookSpan])
  extends Haftarah.Spans(spans)

object Haftarah extends WithBookSpans[Tanach.ProphetsBook] {
  override type Many = Haftarah

  override def apply(spans: Seq[BookSpan]): Haftarah = new Haftarah(spans)

  def toLanguageString(spans: Seq[BookSpan])(implicit spec: LanguageSpec): String = {
    Util.group(spans, (span: BookSpan) => span.book)
      .map { bookSpans =>
        bookSpans.head.book.toLanguageString + " " + bookSpans.map(_.span.toLanguageString).mkString(", ")
      }.mkString("; ")
  }

  override protected def getBook(name: String): Tanach.ProphetsBook = Tanach.getProhetForName(name)

  final def forParsha(parsha: Parsha): Customs = haftarah(parsha).map(_.from(parsha))

  private lazy val haftarah: Map[Parsha, Customs] = Util.mapValues(Metadata.loadMetadata(
    keys = Parsha.values,
    obj = this,
    elementName = "week",
    resourceName = Some("Haftarah")
  ))(metadata => parse(metadata.attributes, metadata.elements, full = true))

  def parse(attributes: Attributes, elements: Seq[Elem], full: Boolean): Customs = {
    val span: BookSpanParsed = parseSpan(attributes)
    val (partElements, customElements) = XML.span(elements, "part", "custom")

    val common: Option[Haftarah] =
      if (partElements.isEmpty && customElements.isEmpty) Some(oneSpan(span)) else
      if (partElements.isEmpty) None else Some(parseParts(partElements, span))

    val customs: Custom.Of[Haftarah] = Custom.Of(customElements.map(parseCustom(span)), full = false)

    val result = common.fold(customs.customs) { common =>
      require(customs.find(Custom.Common).isEmpty)
      customs.customs.updated(Custom.Common, common)
    }

    new Custom.Of(result, full = full)
  }

  private def parseCustom(ancestorSpan: BookSpanParsed)(element: Elem): (Set[Custom], Haftarah) = {
    val (customs, attributes, elements) =  {
      val (attributes, elements) = XML.open(element, "custom")
      val customs: Set[Custom] = Custom.parse(attributes.doGet("n"))
      (customs, attributes, elements)
    }
    customs -> parseCustom(ancestorSpan, attributes, elements)
  }

  private def parseCustom(
    ancestorSpan: BookSpanParsed,
    attributes: Attributes,
    elements: Seq[Elem]
  ): Haftarah = {
    val span = parseSpan(attributes).inheritFrom(ancestorSpan)
    val partElements = XML.span(elements, "part")

    if (partElements.isEmpty) oneSpan(span) else parseParts(partElements, span)
  }

  private def oneSpan(span: BookSpanParsed): Haftarah = Haftarah(Seq(span.resolve))

  private def parseParts(elements: Seq[Elem], ancestorSpan: BookSpanParsed): Haftarah = {
    val result: Seq[WithNumber[BookSpan]] = elements.map { element =>
      WithNumber.parse(
        attributes = XML.openEmpty(element, "part"),
        attributes => parseSpan(attributes).inheritFrom(ancestorSpan).resolve
      )
    }

    WithNumber.checkConsecutive(result, "part")
    require(result.length > 1)
    Haftarah(WithNumber.dropNumbers(result))
  }
}
