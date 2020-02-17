package org.podval.judaica.tanach

import cats.implicits._
import org.digitaljudaica.metadata.{Names, WithNumber}
import org.digitaljudaica.xml.{Parser, Xml}

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

    override def resolve: PsalmsMetadata = new PsalmsMetadata(
      book,
      days,
      weekDays,
      books
    )
  }

  def parser(book: Tanach.Psalms.type, names: Names, chapters: Chapters): Parser[Parsed] = for {
    days <- spansParser(chapters, "day", 30)
    weekDays <- spansParser(chapters, "weekDay", 7)
    books <- spansParser(chapters, "book", 5)
  } yield new Parsed(book, names, chapters, days, weekDays, books)

  private def spansParser(chapters: Chapters, name: String, number: Int): Parser[Seq[Span]] = for {
    numbered <- Xml.element.empty.all(name, WithNumber.parse(SpanParsed.parser))
  } yield {
    val spans: Seq[SpanParsed] = WithNumber.dropNumbers(WithNumber.checkNumber(numbered, number, name))
    SpanSemiResolved.setImpliedTo(spans.map(_.semiResolve), chapters.full, chapters)
  }
}
