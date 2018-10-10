package org.podval.judaica.metadata.tanach

final case class Aliyot(span: BookSpan.ChumashSpan.BookSpan, aliyot: Seq[Span]) {
  // TODO require()
}

final case class AliyotCustom(span: BookSpan.ChumashSpan.BookSpan, aliyot: Custom.Of[Seq[Span]]) {
  // TODO require()
}

object Aliyot {

  // TODO need AliyotCustom.empty() in Tanach.combineDays()
  def processDays(
    book: Tanach.ChumashBook,
    days: Custom.Sets[Seq[Span.Numbered]],
    span: Span
  ): Custom.Of[Seq[Span]] = {
    val chapters = book.chapters
    val withImplied1 = addImplied1(Custom.common(days), span, chapters)

    val result: Custom.Sets[Seq[Span]] = days.mapValues { spans: Seq[Span.Numbered] =>
      val overlayedSpans = WithNumber.overlay(withImplied1, spans)
      setImpliedToCheckAndDropNumbers(overlayedSpans, 7, span, chapters)
    }

    Custom.denormalize(result)
  }

  def parseAliyot(
    parsha: Parsha,
    aliyot: Seq[Span.Numbered]
  ): Aliyot = {
    val book = parsha.book
    val chapters = book.chapters

    val span = Span(parsha.span.from, aliyot.last.span.to.getOrElse(parsha.days(Custom.Common).head.to))

    val withImplied1: Seq[Span.Numbered] = addImplied1(aliyot, span, chapters)
    val result: Seq[Span] = setImpliedToCheckAndDropNumbers(withImplied1, 3, span, chapters)

    Aliyot(
      span = BookSpan.ChumashSpan.BookSpan(book, span),
      aliyot = result
    )
  }

  def parseMaftir(
    parsha: Parsha,
    maftir: Span.SemiResolved
  ): Span = {
    val book = parsha.book
    val chapters = book.chapters
    val span = Span(maftir.from, maftir.to.getOrElse(parsha.span.to))

    Span.setImpliedTo(
      Seq(maftir),
      span,
      chapters
    ).head
  }

  private def addImplied1(
    spans: Seq[Span.Numbered],
    span: Span,
    chapters: Chapters
  ): Seq[Span.Numbered] = {
    val first = spans.head
    val implied: Seq[Span.Numbered] = if (first.n == 1) Seq.empty else Seq(Span.Numbered(1, Span.SemiResolved(
      span.from,
      Some(chapters.prev(first.span.from).get)
    )))

    implied ++ spans
  }

  private def setImpliedToCheckAndDropNumbers(
    spans: Seq[Span.Numbered],
    number: Int,
    span: Span,
    chapters: Chapters
  ): Seq[Span] = Span.setImpliedTo(dropNumbers(WithNumber.checkNumber(spans, number, "span")), span, chapters)

  private def dropNumbers(spans: Seq[Span.Numbered]): Seq[Span.SemiResolved] = spans.map(_.span)
}
