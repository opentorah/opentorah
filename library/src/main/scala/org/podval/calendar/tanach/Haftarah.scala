package org.podval.calendar.tanach

import cats.implicits._
import org.digitaljudaica.metadata.{Attributes, LanguageSpec, Metadata, Xml}
import org.digitaljudaica.util.Collections
import org.podval.judaica.tanach.{Custom, Parsha, Tanach, WithBookSpans, WithNumber}
import scala.xml.Elem

final case class Haftarah private(override val spans: Seq[Haftarah.BookSpan])
  extends Haftarah.Spans(spans)

object Haftarah extends WithBookSpans[Tanach.ProphetsBook] {
  override type Many = Haftarah

  override def apply(spans: Seq[BookSpan]): Haftarah = new Haftarah(spans)

  def toLanguageString(spans: Seq[BookSpan])(implicit spec: LanguageSpec): String = {
    Collections.group(spans, (span: BookSpan) => span.book)
      .map { bookSpans =>
        bookSpans.head.book.toLanguageString + " " + bookSpans.map(_.span.toLanguageString).mkString(", ")
      }.mkString("; ")
  }

  override protected def getBook(name: String): Tanach.ProphetsBook = Tanach.getProhetForName(name)

  final def forParsha(parsha: Parsha): Customs = haftarah(parsha).map(_.from(parsha))

  private lazy val haftarah: Map[Parsha, Customs] = Collections.mapValues(Metadata.loadMetadata(
    keys = Parsha.values,
    obj = this,
    elementName = "week",
    resourceName = Some("Haftarah")
  ))(metadata => parse(metadata.attributes, metadata.elements, full = true))

  def parse(attributes: Attributes, elements: Seq[Elem], full: Boolean): Customs = {
    val span: BookSpanParsed = parseSpan(attributes)
    val (partElements, customElements) = Xml.span(elements, "part", "custom")

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

  private def parseCustom(ancestorSpan: BookSpanParsed)(element: Elem): (Set[Custom], Haftarah) =
    Xml.runA(element, "custom", customParser(ancestorSpan))

  private def customParser(ancestorSpan: BookSpanParsed): Xml.Parser[(Set[Custom], Haftarah)] = for {
    n <- Xml.attribute("n")
    customs = Custom.parse(n)
    bookSpanParsed <- parseSpanNg
    span = bookSpanParsed.inheritFrom(ancestorSpan)
    partElements <- Xml.getElements("part")
    result <- if (partElements.isEmpty) Xml.pure(oneSpan(span)) else parsePartsNg(partElements, span)
  } yield customs -> result


  private def oneSpan(span: BookSpanParsed): Haftarah = Haftarah(Seq(span.resolve))

  private def parseParts(elements: Seq[Elem], ancestorSpan: BookSpanParsed): Haftarah = {
    val result: Seq[WithNumber[BookSpan]] = elements.map { element =>
      WithNumber.parse(
        attributes = Xml.openEmpty(element, "part"),
        attributes => parseSpan(attributes).inheritFrom(ancestorSpan).resolve
      )
    }

    WithNumber.checkConsecutive(result, "part")
    require(result.length > 1)
    Haftarah(WithNumber.dropNumbers(result))
  }


  private def parsePartsNg(elements: Seq[Elem], ancestorSpan: BookSpanParsed): Xml.Parser[Haftarah] = for {
    result <- Xml.elements(elements, "part", WithNumber.parseNg(parsePart(ancestorSpan)))
    _ <- WithNumber.checkConsecutiveNg(result, "part")
    _ <- Xml.check(result.length > 1, "too short")
  } yield Haftarah(WithNumber.dropNumbers(result))


  private def parsePart(ancestorSpan: BookSpanParsed): Xml.Parser[BookSpan] = for {
    result <- parseSpanNg
  } yield result.inheritFrom(ancestorSpan).resolve
}
