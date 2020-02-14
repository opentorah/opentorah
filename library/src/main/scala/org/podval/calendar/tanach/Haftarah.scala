package org.podval.calendar.tanach

import cats.implicits._
import org.digitaljudaica.metadata.{LanguageSpec, Metadata, WithNumber}
import org.digitaljudaica.util.Collections
import org.digitaljudaica.xml.{Attribute, Element, From, Parser}
import org.podval.judaica.tanach.{Custom, Parsha, Tanach, WithBookSpans}

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

  private lazy val haftarah: Map[Parsha, Customs] = Metadata.metadataUsingNames(
    keys = Parsha.values,
    from = From.resource(this),
    elementName = "week",
    parser(true)
  )

  def parser(full: Boolean): Parser[Customs] = for {
    span <- spanParser
    hasParts <- Element.nextNested.nameIs("part")
    parts <- if (!hasParts) Parser.pure(None) else partsParser(span).map(Some(_))
    hasCustom <- Element.nextNested.nameIs("custom")
    customs <- Element.all("custom", customParser(span)).map(customs => Custom.Of(customs, full = false))
  } yield {
    val common: Option[Haftarah] = if (!hasParts && !hasCustom) Some(oneSpan(span)) else parts

    val result = common.fold(customs.customs) { common =>
      require(customs.find(Custom.Common).isEmpty)
      customs.customs.updated(Custom.Common, common)
    }

    new Custom.Of(result, full = full)
  }

  private def oneSpan(span: BookSpanParsed): Haftarah = Haftarah(Seq(span.resolve))

  private def customParser(ancestorSpan: BookSpanParsed): Parser[(Set[Custom], Haftarah)] = for {
    n <- Attribute.required("n")
    bookSpanParsed <- spanParser.map(_.inheritFrom(ancestorSpan))
    hasParts <- Element.nextNested.nameIs("part")
    result <- if (!hasParts) Parser.pure[Haftarah](oneSpan(bookSpanParsed)) else partsParser(bookSpanParsed)
  } yield Custom.parse(n) -> result

  private def partsParser(ancestorSpan: BookSpanParsed): Parser[Haftarah] = for {
    parts <- Element.all("part", WithNumber.parse(
      spanParser.map(_.inheritFrom(ancestorSpan).resolve)))
    _ <- WithNumber.checkConsecutiveNg(parts, "part")
    _ <- Parser.check(parts.length > 1, "too short")
  } yield Haftarah(WithNumber.dropNumbers(parts))
}
