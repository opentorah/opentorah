package org.podval.judaica.metadata.tanach

// Other than on Simchas Torah, aliyot are from the same book.
final case class Torah private(override val spans: Seq[Torah.BookSpan])
  extends Torah.Spans(spans)
{
  def to6withLast(last: Torah.Aliyah): Torah =
    Torah(spans.take(5) :+ (spans(5)+spans(6)) :+ last)
}

object Torah extends WithBookSpans[Tanach.ChumashBook] {
  protected override type Many = Torah

  override def apply(spans: Seq[BookSpan]): Torah = new Torah(spans)

  type Fragment = BookSpan

  type Aliyah = BookSpan

  type Maftir = BookSpan

  override protected def getBook(name: String): Tanach.ChumashBook = Tanach.getChumashForName(name)

  def aliyot(spans: BookSpan*): Torah = Torah(spans)

  // TODO def toStringWithNumbers()

  type Numbered = WithNumber[SpanSemiResolved]

  def parseAliyot(
    bookSpan: BookSpan,
    aliyotRaw: Seq[Numbered],
    number: Option[Int]
  ): Torah = {
    val span: Span = bookSpan.span
    val chapters: Chapters = bookSpan.book.chapters
    val with1: Seq[Numbered] = addImplied1(aliyotRaw, span, chapters)
    val spans: Seq[Numbered] = WithNumber.checkNumber(with1, number.getOrElse(with1.length), "span")
    // TODO verify that spans are consecutive and cover the book span - only when parsing!
    val result: Seq[Span] = setImpliedTo(WithNumber.dropNumbers(spans), span, chapters)
    Torah(result.map(inBook(bookSpan.book, _)))
  }

  def inBook(book: Tanach.ChumashBook, span: Span): BookSpan = BookSpan(book, span)

  def setImpliedTo(
    spans: Seq[SpanSemiResolved],
    span: Span,
    chapters: Chapters
  ): Seq[Span] = {
    val tos: Seq[Verse] = spans.tail.map(_.from).map(chapters.prev(_).get) :+ span.to
    val result = spans.zip(tos).map { case (s, to) => s.setTo(to) }
    require(chapters.cover(result, span))
    result
  }

  def processDays(
    book: Tanach.ChumashBook,
    days: Custom.Sets[Seq[Numbered]],
    span: Span
  ): Torah.Customs = {
    val bookSpan = inBook(book, span)
    val with1 = addImplied1(Custom.common(days), span, book.chapters)

    val result: Custom.Sets[Torah] = days.mapValues { spans: Seq[Numbered] =>
      parseAliyot(bookSpan, WithNumber.overlay(with1, spans), Some(7))
    }

    Custom.Of(result)
  }

  private def addImplied1(
    spans: Seq[Numbered],
    span: Span,
    chapters: Chapters
  ): Seq[Numbered] = {
    val first = spans.head
    val implied1: Seq[Numbered] = if (first.n == 1) Seq.empty else Seq(new Numbered(1, SpanSemiResolved(
      span.from,
      Some(chapters.prev(first.what.from).get)
    )))

    implied1 ++ spans
  }
}
