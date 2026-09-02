package org.opentorah.texts.tanach

import org.opentorah.calendar.Week
import org.opentorah.metadata.Names
import org.opentorah.store.By
import org.opentorah.util.Effects
import org.podval.xml.XmlAst
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

  override def parse[E: XmlAst](names: Names, chapters: Chapters, element: E): PsalmsBook.Parsed =
    PsalmsBook.parse(this, names, chapters, element)

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

    override def resolve: Effects.IO[Metadata] = ZIO.succeed(Metadata(
      days,
      weekDays,
      books
    ))

  def parse[E: XmlAst](book: PsalmsBook, names: Names, chapters: Chapters, element: E): Parsed =
    XmlDecode.requireNoOther(element, Set("name", "chapter", "day", "weekDay", "book"))
    Parsed(
      names,
      chapters,
      days = spans(element, "day", 30, chapters),
      weekDays = spans(element, "weekDay", 7, chapters),
      books = spans(element, "book", 5, chapters)
    )

  private def spans[E: XmlAst](element: E, name: String, number: Int, chapters: Chapters): Seq[Span] =
    val numbered: Seq[WithNumber[SpanParsed]] =
      XmlDecode.childrenNamed(element, name).map(el => WithNumber.decode(el, SpanParsed.decode))
    require(numbered.map(_.n) == (1 to numbered.length), s"Wrong $name numbers: $numbered")
    require(numbered.length == number, s"Wrong number of ${name}s: ${numbered.length} != $number")
    SpanSemiResolved.setImpliedTo(WithNumber.dropNumbers(numbered).map(_.semiResolve), chapters.full, chapters)

