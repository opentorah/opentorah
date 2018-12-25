package org.podval.judaica.tanach

import org.podval.judaica.metadata.WithNames

// Other than on Simchas Torah, aliyot are from the same book.
final case class Torah private(override val spans: Seq[Torah.BookSpan])
  extends Torah.Spans(spans)
{
  def drop(toDrop: Set[Int]): Torah = {
    def drop(what: Seq[(Torah.Aliyah, Boolean)]): Seq[Torah.Aliyah] = what match {
      case (a1, d1) :: (a2, d2) :: tail =>
        if (d2) drop((a1+a2, d1) +: tail)
        else a1 +: drop((a2, d2) +: tail)
      case (a1, d1) :: Nil =>
        Seq(a1)
    }

    val withDrop = spans.zipWithIndex.map { case (a, i) => (a, toDrop.contains(i+1)) }
    require(!withDrop.head._2)
    Torah(drop(withDrop))
  }

  def fromWithNumbers(source: WithNames): Torah =  {
    val result = spans.zipWithIndex.map { case (aliyah, index) =>
      aliyah.from(new Source.AndNumber(withNames = source, number = index + 1))
    }

    Torah(result)
  }

  def to6withLast(last: Torah.Aliyah): Torah = drop(Set(7)) :+ last
}

object Torah extends WithBookSpans[Tanach.ChumashBook] {
  protected override type Many = Torah

  override def apply(spans: Seq[BookSpan]): Torah = new Torah(spans)

  type Fragment = BookSpan

  type Aliyah = BookSpan

  type Maftir = BookSpan

  override protected def getBook(name: String): Tanach.ChumashBook = Tanach.getChumashForName(name)

  def aliyot(spans: BookSpan*): Torah = Torah(spans)

  type Numbered = WithNumber[SpanSemiResolved]

  def parseAliyot(
    bookSpan: BookSpan,
    aliyotRaw: Seq[Numbered],
    number: Option[Int]
  ): Torah = {
    val span: Span = bookSpan.span
    val chapters: Chapters = bookSpan.book.chapters
    val with1: Seq[Numbered] = addImplied1(aliyotRaw, span, chapters)
    val numberedSpans: Seq[Numbered] = WithNumber.checkNumber(with1, number.getOrElse(with1.length), "span")
    val spans: Seq[Span] = setImpliedTo(WithNumber.dropNumbers(numberedSpans), span, chapters)
    require(bookSpan.book.chapters.consecutive(spans))
    val bookSpans: Seq[Aliyah] = spans.map(inBook(bookSpan.book, _))
    Torah(bookSpans)
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
