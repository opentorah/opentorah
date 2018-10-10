package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.LanguageSpec

final case class Aliyot(span: BookSpan.ChumashSpan.BookSpan, aliyot: Seq[Span]) {
  // TODO verify that spans are consecutive and cover the book span

  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String = aliyot.zipWithIndex.map { case (span, index) =>
    s"${index+1}: $span"
  }.mkString("\n")
}

object Aliyot {

  def processDays(
    book: Tanach.ChumashBook,
    days: Custom.Sets[Seq[Span.Numbered]],
    span: Span
  ): Custom.Of[Aliyot] = {
    val chapters = book.chapters
    val withImplied1 = addImplied1(Custom.common(days), span, chapters)

    val result: Custom.Sets[Seq[Span]] = days.mapValues { spans: Seq[Span.Numbered] =>
      val overlayedSpans = WithNumber.overlay(withImplied1, spans)
      setImpliedToCheckAndDropNumbers(overlayedSpans, 7, span, chapters)
    }

    val bookSpan = span.inBook(book)
    Custom.denormalize(result).mapValues(aliyot => Aliyot(bookSpan, aliyot))
  }

  def parseAliyot(
    parsha: Parsha,
    aliyot: Seq[Span.Numbered]
  ): Aliyot = {
    val book = parsha.book
    val chapters = book.chapters

    val day1common = parsha.days(Custom.Common).aliyot.head
    val span = Span(parsha.span.from, aliyot.last.span.to.getOrElse(day1common.to))

    val withImplied1: Seq[Span.Numbered] = addImplied1(aliyot, span, chapters)
    val result: Seq[Span] = setImpliedToCheckAndDropNumbers(withImplied1, 3, span, chapters)

    Aliyot(
      span = span.inBook(book),
      aliyot = result
    )
  }

  def parseAliyot(
    bookSpan: BookSpan.ChumashSpan.BookSpan,
    aliyot: Seq[Span.Numbered]
  ): Aliyot = {
    val span = bookSpan.span
    val chapters = bookSpan.book.chapters
    val withImplied1: Seq[Span.Numbered] = addImplied1(aliyot, span, chapters)
    val result: Seq[Span] = setImpliedToCheckAndDropNumbers(withImplied1, withImplied1.length, span, chapters)

    Aliyot(
      span = bookSpan,
      aliyot = result
    )
  }

  def parseMaftir(
    parsha: Parsha,
    maftir: Span.SemiResolved
  ): BookSpan.ChumashSpan.BookSpan = {
    val book = parsha.book
    val chapters = book.chapters
    val span = Span(maftir.from, maftir.to.getOrElse(parsha.span.to))

    Span.setImpliedTo(
      Seq(maftir),
      span,
      chapters
    ).head.inBook(parsha.book)
  }

  private def addImplied1(
    spans: Seq[Span.Numbered],
    span: Span,
    chapters: Chapters
  ): Seq[Span.Numbered] = {
    val first = spans.head
    val implied1: Seq[Span.Numbered] = if (first.n == 1) Seq.empty else Seq(Span.Numbered(1, Span.SemiResolved(
      span.from,
      Some(chapters.prev(first.span.from).get)
    )))

    implied1 ++ spans
  }

  private def setImpliedToCheckAndDropNumbers(
    spans: Seq[Span.Numbered],
    number: Int,
    span: Span,
    chapters: Chapters
  ): Seq[Span] = Span.setImpliedTo(dropNumbers(WithNumber.checkNumber(spans, number, "span")), span, chapters)

  private def dropNumbers(spans: Seq[Span.Numbered]): Seq[Span.SemiResolved] = spans.map(_.span)
}
