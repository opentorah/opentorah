package org.opentorah.texts.tanach

import org.opentorah.metadata.{WithNames, WithNumber}
import org.opentorah.xml.{Element, Parser}

// Other than on Simchas Torah, aliyot are from the same book.
final case class Torah private(override val spans: Seq[Torah.BookSpan])
  extends Torah.Spans(spans)
{
  def drop(toDrop: Set[Int]): Torah = {
    def drop(what: Seq[(Torah.Aliyah, Boolean)]): Seq[Torah.Aliyah] = what match {
      case (a1, d1) :: (a2, d2) :: tail =>
        if (d2) drop((a1+a2, d1) +: tail)
        else a1 +: drop((a2, d2) +: tail)
      case (a1, _) :: Nil =>
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
    aliyot: Seq[Numbered],
    number: Option[Int]
  ): Parser[Torah] = {
    val span: Span = bookSpan.span
    val chapters: Chapters = bookSpan.book.chapters
    val with1: Seq[Numbered] = addImplied1(aliyot, span, chapters)
    for {
      _ <- WithNumber.checkNumber(with1, number.getOrElse(with1.length), "span")
      spans: Seq[Span] = SpanSemiResolved.setImpliedTo(WithNumber.dropNumbers(with1), span, chapters)
      _ <- Parser.check(bookSpan.book.chapters.consecutive(spans), s"Non-consecutive: $spans")
    } yield Torah(spans.map(inBook(bookSpan.book, _)))
  }

  val torahParsable: Element[Torah] = new Element[Torah]("torah") {
    override protected def parser: Parser[Torah] = for {
      bookSpan <- spanParser.map(_.resolve)
      spans <- aliyahParsable(bookSpan).all
      result <- parseAliyot(bookSpan, spans, number = None)
    } yield result
  }

  private def aliyahParsable(bookSpan: BookSpan): Element[Numbered] = new Element[Numbered]("aliyah") {
    override protected def parser: Parser[WithNumber[SpanSemiResolved]] =
      WithNumber.parse(SpanParsed.parser.map(_.defaultFromChapter(bookSpan.span.from.chapter).semiResolve))
  }

  val maftirParsable: Element[BookSpan] = new Element[BookSpan]("maftir") {
    override protected def parser: Parser[BookSpan] = spanParser.map(_.resolve)
  }

  def inBook(book: Tanach.ChumashBook, span: Span): BookSpan = BookSpan(book, span)

  def processDays(
    book: Tanach.ChumashBook,
    days: Custom.Sets[Seq[Numbered]],
    span: Span
  ): Parser[Torah.Customs] = {
    val bookSpan = inBook(book, span)
    val with1 = addImplied1(Custom.common(days), span, book.chapters)

    val result: Parser[Custom.Sets[Torah]] = Parser.mapValues(days){ spans: Seq[Numbered] =>
      parseAliyot(bookSpan, WithNumber.overlay(with1, spans), Some(7))
    }

    result.map(Custom.Of(_))
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
