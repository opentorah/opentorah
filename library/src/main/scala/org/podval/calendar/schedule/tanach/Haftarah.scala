package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.{WithBookSpans, Tanach, Custom, Parsha, WithNumber}
import org.podval.judaica.metadata.{Attributes, LanguageSpec, LanguageString, Metadata, Util, XML}

import scala.xml.Elem

final case class Haftarah private(override val spans: Seq[Haftarah.BookSpan])
  extends Haftarah.Spans(spans) with LanguageString
{
  override def toLanguageString(implicit spec: LanguageSpec): String = {
    Util.group(spans, (span: Haftarah.BookSpan) => span.book)
      .map { bookSpans =>
        bookSpans.head.book.toLanguageString + " " + bookSpans.map(_.span.toLanguageString).mkString(", ")
      }.mkString("; ")
  }
}

object Haftarah extends WithBookSpans[Tanach.ProphetsBook] {
  override type Many = Haftarah

  override def apply(spans: Seq[BookSpan]): Haftarah = new Haftarah(spans)

  override protected def getBook(name: String): Tanach.ProphetsBook = Tanach.getProhetForName(name)

  final def forParsha(parsha: Parsha): Customs = haftarah(parsha)

  private lazy val haftarah: Map[Parsha, Customs] = Metadata.loadMetadata(
    keys = Parsha.values,
    obj = this,
    elementName = "week",
    resourceName = Some("Haftarah")
  ).mapValues { metadata => parse(metadata.attributes, metadata.elements, full = true) }

  def parse(element: Elem, full: Boolean): Customs = {
    val (attributes, elements) = XML.open(element, "haftarah")
    parse(attributes, elements, full = full)
  }

  private def parse(attributes: Attributes, elements: Seq[Elem], full: Boolean): Customs = {
    val span: BookSpanParsed = parseSpan(attributes)
    val (partElements, customElements) = XML.span(elements, "part", "custom")

    val common: Option[Haftarah] =
      if (partElements.isEmpty && customElements.isEmpty) Some(oneSpan(span)) else
      if (partElements.isEmpty) None else Some(parseParts(partElements, span))

    // TODO toMap() will silently ignore duplicates...
    val customs: Custom.Sets[Haftarah] = customElements.map(parseCustom(span)).toMap

    val result: Custom.Sets[Haftarah] = common.fold(customs) { common =>
      // TODO updated() will silently ignore duplicates...
      customs.updated(Set[Custom](Custom.Common), common)
    }

    Custom.Of(result, full)
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
