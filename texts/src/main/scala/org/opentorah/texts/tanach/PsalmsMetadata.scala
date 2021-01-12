package org.opentorah.texts.tanach

import org.opentorah.metadata.{Names, WithNumber}
import org.opentorah.xml.{Unparser, ContentType, Element, Parsable, Parser}
import zio.ZIO

final class PsalmsMetadata(
  book: Tanach.Psalms.type,
  val days: Seq[Span],
  val weekDays: Seq[Span],
  val books: Seq[Span]
) extends TanachBookMetadata(book)

object PsalmsMetadata {

  final class Parsed(
     book: Tanach.Psalms.type,
     names: Names,
     chapters: Chapters,
     val days: Seq[Span],
     val weekDays: Seq[Span],
     val books: Seq[Span]
  ) extends TanachBookMetadata.Parsed(book, names, chapters) {

    override def resolve: Parser[PsalmsMetadata] = ZIO.succeed(new PsalmsMetadata(
      book,
      days,
      weekDays,
      books
    ))
  }

  def parser(book: Tanach.Psalms.type, names: Names, chapters: Chapters): Parser[Parsed] = for {
    days <- spansParser(chapters, "day", 30)
    weekDays <- spansParser(chapters, "weekDay", 7)
    books <- spansParser(chapters, "book", 5)
  } yield new Parsed(book, names, chapters, days, weekDays, books)

  private def spansParser(chapters: Chapters, name: String, number: Int): Parser[Seq[Span]] = for {
    numbered <- new SpanParsable(name).seq()
    _ <- WithNumber.checkNumber(numbered, number, name)
  } yield SpanSemiResolved.setImpliedTo(WithNumber.dropNumbers(numbered).map(_.semiResolve), chapters.full, chapters)

  private final class SpanParsable(name: String) extends Element[WithNumber[SpanParsed]](name) {
    override def contentType: ContentType = ContentType.Empty

    override def contentParsable: Parsable[WithNumber[SpanParsed]] = new Parsable[WithNumber[SpanParsed]] {
      override def parser: Parser[WithNumber[SpanParsed]] = WithNumber.parse(SpanParsed.parser)
      override def unparser: Unparser[WithNumber[SpanParsed]] = ???
    }
  }
}
