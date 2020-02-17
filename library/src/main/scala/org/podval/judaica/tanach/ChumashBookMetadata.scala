package org.podval.judaica.tanach

import cats.implicits._
import org.digitaljudaica.metadata.{Metadata, Names}
import org.digitaljudaica.util.Collections
import org.digitaljudaica.xml.{Parser, Xml}

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

    def resolve: ChumashBookMetadata = {
      // TODO re-read and understand ;)
      val parsha2metadataParsed = Metadata.toMap(book.parshiot, weeks, (metadata: ParshaMetadata.Parsed) => metadata.parsha)

      val chaptersFull = chapters.full
      val parsha2span: Map[Parsha, Span] = Collections.inSequence(
        keys = book.parshiot,
        map = Collections.mapValues(parsha2metadataParsed)(_.span),
        f = (pairs: Seq[(Parsha, SpanSemiResolved)]) =>
          SpanSemiResolved.setImpliedTo(pairs.map(_._2), chaptersFull, chapters)
      )

      val parsha2daysCombined: Map[Parsha, Option[Torah.Customs]] = Collections.inSequence(
        keys = book.parshiot,
        map = Collections.mapValues(parsha2metadataParsed)(_.daysCombined),
        f = combineDays(parsha2span, _)
      )

      val parsha2metadata = Collections.mapValues(parsha2metadataParsed)(metadata => metadata.resolve(
        parshaSpan = parsha2span(metadata.parsha),
        daysCombined = parsha2daysCombined(metadata.parsha)
      ))

      new ChumashBookMetadata(book, parsha2metadata)
    }

    private def combineDays(parsha2span: Map[Parsha, Span],
      weeks: Seq[(Parsha, Custom.Sets[Seq[Torah.Numbered]])]
    ): Seq[Option[Torah.Customs]] = weeks match {
      case (parsha, days) :: (parshaNext, daysNext) :: tail =>
        val result: Option[Torah.Customs] = if (!parsha.combines) None else  {
          val combined: Custom.Sets[Seq[Torah.Numbered]] = daysNext ++ days.map { case (customs, value) =>
            (customs, value ++ daysNext.getOrElse(customs, Seq.empty))
          }

          val book = parsha.book
          Some(Torah.processDays(
            book,
            combined,
            book.chapters.merge(
              parsha2span(parsha),
              parsha2span(parshaNext)
            )))
        }

        result +: combineDays(parsha2span, (parshaNext, daysNext) +: tail)

      case (parsha, _ /*days*/) :: Nil =>
        require(!parsha.combines)
        Seq(None)

      case Nil => Nil
    }
  }

  def parser(book: Tanach.ChumashBook, names: Names, chapters: Chapters): Parser[Parsed] = for {
    weeks <- Xml.element.elements.all("week", ParshaMetadata.parser(book))
  } yield new Parsed(book, names, chapters, weeks)
}
