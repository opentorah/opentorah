package org.opentorah.texts.tanach

import org.opentorah.calendar.Week
import org.opentorah.metadata.Names
import org.opentorah.store.{By, Store}
import org.opentorah.xml.{Element, Parsable, Parser, Unparser}
import zio.ZIO
import Tanach.Psalms

trait PsalmsBook extends NachBook:
  def days: Seq[Span] = metadata.days

  def weekDays: Seq[Span] = metadata.weekDays

  def books: Seq[Span] = metadata.books

  private def metadata: PsalmsBook.Metadata = TanachBook.metadata(Psalms).asInstanceOf[PsalmsBook.Metadata]

  override def storesPure: Seq[By[?]] = Seq(
    chapters.byChapter,
    // TODO override to/from name(s)/number
    Chapters.BySpan("book", books, chapters),
    Chapters.BySpan("day", days, chapters),
    new Chapters.BySpan("day of the week", weekDays, chapters):
      // Recognize names of the days of the week:
      override def name2number(name: String): Option[Int] = super.name2number(name)
        .orElse(Week.Day.forDefaultName(name).map(_.ordinal + 1))
  )

  override def parser(names: Names, chapters: Chapters): Parser[PsalmsBook.Parsed] =
    PsalmsBook.parser(this, names, chapters)

object PsalmsBook:
  final class Metadata(
    val days: Seq[Span],
    val weekDays: Seq[Span],
    val books: Seq[Span]
  ) extends NachBook.Metadata(Psalms)

  final class Parsed(
    names: Names,
    chapters: Chapters,
    val days: Seq[Span],
    val weekDays: Seq[Span],
    val books: Seq[Span]
  ) extends NachBook.Parsed(Psalms, names, chapters):

    override def resolve: Parser[Metadata] = ZIO.succeed(Metadata(
      days,
      weekDays,
      books
    ))

  def parser(book: PsalmsBook, names: Names, chapters: Chapters): Parser[Parsed] = for
    days: Seq[Span] <- spansParser(chapters, "day", 30)
    weekDays: Seq[Span] <- spansParser(chapters, "weekDay", 7)
    books: Seq[Span] <- spansParser(chapters, "book", 5)
  yield Parsed(
    names,
    chapters,
    days,
    weekDays,
    books
  )

  private def spansParser(chapters: Chapters, name: String, number: Int): Parser[Seq[Span]] = for
    numbered: Seq[WithNumber[SpanParsed]] <- SpanParsable(name).seq()
    _ <- WithNumber.checkNumber(numbered, number, name)
  yield SpanSemiResolved.setImpliedTo(WithNumber.dropNumbers(numbered).map(_.semiResolve), chapters.full, chapters)

  private final class SpanParsable(name: String) extends Element[WithNumber[SpanParsed]](name):
    override def contentType: Element.ContentType = Element.ContentType.Empty

    override def contentParsable: Parsable[WithNumber[SpanParsed]] = new Parsable[WithNumber[SpanParsed]]:
      override def parser: Parser[WithNumber[SpanParsed]] = WithNumber.parse(SpanParsed.parser)
      override def unparser: Unparser[WithNumber[SpanParsed]] = ???

