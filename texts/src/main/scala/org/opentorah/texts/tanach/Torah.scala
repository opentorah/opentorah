package org.opentorah.texts.tanach

import org.opentorah.metadata.{WithNames, WithNumber}
import org.opentorah.util.Effects
import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

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

  def fromWithNumbers(source: WithNames): Torah =  Torah {
    spans.zipWithIndex.map { case (aliyah, index) =>
      aliyah.from(new Source.AndNumber(withNames = source, number = index + 1))
    }
  }
}

object Torah extends WithBookSpans[Chumash] {
  protected override type Many = Torah

  override def apply(spans: Seq[BookSpan]): Torah = new Torah(spans)

  type Fragment = BookSpan

  type Aliyah = BookSpan

  type Maftir = BookSpan

  override protected def getBook(name: String): Chumash = Chumash.forName(name)

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
      _ <- Effects.check(bookSpan.book.chapters.consecutive(spans), s"Non-consecutive: $spans")
    } yield Torah(spans.map(inBook(bookSpan.book, _)))
  }

  object torahParsable extends Element[Torah]("torah") {
    override def contentParsable: Parsable[Torah] = new Parsable[Torah] {
      override def parser: Parser[Torah] = for {
        bookSpan <- spanParser.map(_.resolve)
        spans <- new AliyahParsable(bookSpan).seq()
        result <- parseAliyot(bookSpan, spans, number = None)
      } yield result

      override def unparser: Unparser[Torah] = ???
    }
  }

  private final class AliyahParsable(bookSpan: BookSpan) extends Element[Numbered]("aliyah") {
    override def contentParsable: Parsable[Numbered] = new Parsable[Numbered] {
      override def parser: Parser[WithNumber[SpanSemiResolved]] =
        WithNumber.parse(SpanParsed.parser.map(_.defaultFromChapter(bookSpan.span.from.chapter).semiResolve))

      override def unparser: Unparser[Numbered] = ???
    }
  }

  object Maftir extends Element[BookSpan]("maftir") {
    override def contentParsable: Parsable[BookSpan] = new Parsable[BookSpan] {
      override def parser: Parser[BookSpan] = spanParser.map(_.resolve)
      override def unparser: Unparser[BookSpan] = ???
    }
  }

  def inBook(book: Chumash, span: Span): BookSpan = BookSpan(book, span)

  def processDays(
    book: Chumash,
    days: Custom.Sets[Seq[Numbered]],
    span: Span
  ): Parser[Torah.Customs] = {
    val bookSpan = inBook(book, span)
    val with1 = addImplied1(Custom.common(days), span, book.chapters)

    val result: Parser[Custom.Sets[Torah]] = Effects.mapValues(days){ spans: Seq[Numbered] =>
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
