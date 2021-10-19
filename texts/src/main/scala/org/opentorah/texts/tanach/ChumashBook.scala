package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, Names}
import org.opentorah.store.{By, Pure, Store}
import org.opentorah.util.{Collections, Effects}
import org.opentorah.xml.Parser

trait ChumashBook extends TanachBook:
  lazy val parshiot: Seq[Parsha] = Parsha.forChumash(this)

  override def storesPure: Seq[By[?]] = Seq(
    chapters.byChapter,
    new By.WithSelector[Parsha](selectorName = "parsha")
      with Pure.With[Parsha](storesPure = parshiot)
  )

  // Parsed names of the book are ignored - names of the first parsha are used instead.
  override def names: Names = parshiot.head.names

  override def parser(names: Names, chapters: Chapters): Parser[ChumashBook.Parsed] =
    ChumashBook.parser(this, names, chapters)

  def metadata: ChumashBook.Metadata = TanachBook.metadata(this).asInstanceOf[ChumashBook.Metadata]

object ChumashBook:
  def parser(book: ChumashBook, names: Names, chapters: Chapters): Parser[Parsed] = for
    weeks: Seq[Parsha.Parsed] <- Parsha.WeekParsable(book).seq()
    _ <- Effects.check(names.getDefaultName.isDefined,
      "Only default name is allowed for a Chumash book")
    _ <- Effects.check(weeks.head.names.hasName(names.getDefaultName.get),
      "Chumash book name must be a name of the book's first parsha")
  yield Parsed(book, names, chapters, weeks)
  
  final class Metadata(
    book: ChumashBook,
    parsha2metadata: Map[Parsha, Parsha.ParshaMetadata]
  ) extends TanachBook.Metadata(book):
    def forParsha(parsha: Parsha): Parsha.ParshaMetadata = parsha2metadata(parsha)

  final class Parsed(
    book: ChumashBook,
    names: Names,
    chapters: Chapters,
    weeks: Seq[Parsha.Parsed]
  ) extends TanachBook.Parsed(book, names, chapters):

    def resolve: Parser[Metadata] = for
      parsha2metadataParsed <- HasName.bind[Parsha, Parsha.Parsed](
        keys = book.parshiot,
        metadatas = weeks,
        getKey = _.parsha
      )

      parsha2span: Map[Parsha, Span] = Collections.inSequence(
        keys = book.parshiot,
        map = Collections.mapValues(parsha2metadataParsed)(_.span),
        f = (pairs: Seq[(Parsha, SpanSemiResolved)]) =>
          SpanSemiResolved.setImpliedTo(pairs.map(_._2), chapters.full, chapters)
      )

      parsha2daysCombined: Map[Parsha, Option[Torah.Customs]] = Collections.inSequence(
        keys = book.parshiot,
        map = Collections.mapValues(parsha2metadataParsed)(_.daysCombined),
        f = combineDays(parsha2span, _)
      )

      parsha2metadata <- Effects.mapValues(parsha2metadataParsed)(metadata => metadata.resolve(
        parshaSpan = parsha2span(metadata.parsha),
        daysCombined = parsha2daysCombined(metadata.parsha)
      ))
    yield Metadata(book, parsha2metadata)

    private def combineDays(
      parsha2span: Map[Parsha, Span],
      weeks: Seq[(Parsha, Custom.Sets[Seq[Torah.Numbered]])]
    ): Seq[Option[Torah.Customs]] =
      if weeks.isEmpty then Seq.empty else
      if weeks.length == 1 then
        require(!weeks.head._1.combines)
        Seq(None)
      else
        val (parsha, days) = weeks.head
        val (parshaNext, daysNext) = weeks.tail.head
        val result: Option[Torah.Customs] = if !parsha.combines then None else 
          val combined: Custom.Sets[Seq[Torah.Numbered]] = daysNext ++ days.map((customs, value) =>
            (customs, value ++ daysNext.getOrElse(customs, Seq.empty))
          )

          val book: Tanach.Chumash = parsha.book
          Some(Parser.unsafeRun(Torah.processDays(
            book,
            combined,
            book.chapters.merge(
              parsha2span(parsha),
              parsha2span(parshaNext)
            )
          )))

        result +: combineDays(parsha2span, (parshaNext, daysNext) +: weeks.tail.tail)
