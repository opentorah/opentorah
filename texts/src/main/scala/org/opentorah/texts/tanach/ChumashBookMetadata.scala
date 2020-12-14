package org.opentorah.texts.tanach

import org.opentorah.metadata.{Metadata, Names}
import org.opentorah.util.Collections
import org.opentorah.xml.{Element, Parser}

final class ChumashBookMetadata(
  book: Tanach.ChumashBook,
  parsha2metadata: Map[Parsha, ParshaMetadata]
) extends TanachBookMetadata(book) {

  def forParsha(parsha: Parsha): ParshaMetadata = parsha2metadata(parsha)
}

object ChumashBookMetadata {

  final class Parsed(
    book: Tanach.ChumashBook,
    names: Names,
    chapters: Chapters,
    weeks: Seq[ParshaMetadata.Parsed]
  ) extends TanachBookMetadata.Parsed(book, names, chapters) {

    def resolve: Parser[ChumashBookMetadata] = for {
      parsha2metadataParsed <- Metadata.bind(
        keys = book.parshiot,
        metadatas = weeks,
        getKey = (metadata: ParshaMetadata.Parsed) => metadata.parsha)

      chaptersFull = chapters.full
      parsha2span = Collections.inSequence(
        keys = book.parshiot,
        map = Collections.mapValues(parsha2metadataParsed)(_.span),
        f = (pairs: Seq[(Parsha, SpanSemiResolved)]) =>
          SpanSemiResolved.setImpliedTo(pairs.map(_._2), chaptersFull, chapters)
      )

      parsha2daysCombined: Map[Parsha, Option[Torah.Customs]] = Collections.inSequence(
        keys = book.parshiot,
        map = Collections.mapValues(parsha2metadataParsed)(_.daysCombined),
        f = combineDays(parsha2span, _)
      )

      parsha2metadata <- Parser.mapValues(parsha2metadataParsed)(metadata => metadata.resolve(
        parshaSpan = parsha2span(metadata.parsha),
        daysCombined = parsha2daysCombined(metadata.parsha)
      ))
    } yield new ChumashBookMetadata(book, parsha2metadata)

    private def combineDays(parsha2span: Map[Parsha, Span],
      weeks: Seq[(Parsha, Custom.Sets[Seq[Torah.Numbered]])]
    ): Seq[Option[Torah.Customs]] = weeks match {
      case (parsha, days) :: (parshaNext, daysNext) :: tail =>
        val result: Option[Torah.Customs] = if (!parsha.combines) None else  {
          val combined: Custom.Sets[Seq[Torah.Numbered]] = daysNext ++ days.map { case (customs, value) =>
            (customs, value ++ daysNext.getOrElse(customs, Seq.empty))
          }

          val book = parsha.book
          Some(Parser.parseDo(Torah.processDays(
            book,
            combined,
            book.chapters.merge(
              parsha2span(parsha),
              parsha2span(parshaNext)
            ))))
        }

        result +: combineDays(parsha2span, (parshaNext, daysNext) +: tail)

      case (parsha, _ /*days*/) :: Nil =>
        require(!parsha.combines)
        Seq(None)

      case Nil => Nil
    }
  }

  def parser(book: Tanach.ChumashBook, names: Names, chapters: Chapters): Parser[Parsed] = for {
    weeks <- weekParsable(book).all
    _ <- Parser.check(names.getDefaultName.isDefined,
      "Only default name is allowed for a Chumash book")
    _ <- Parser.check(weeks.head.names.hasName(names.getDefaultName.get),
      "Chumash book name must be a name of the book's first parsha")
  } yield new Parsed(book, names, chapters, weeks)

  private def weekParsable(book: Tanach.ChumashBook): Element[ParshaMetadata.Parsed] =
    new Element[ParshaMetadata.Parsed]("week") {
      override def parser: Parser[ParshaMetadata.Parsed] = ParshaMetadata.parser(book)
    }
}
