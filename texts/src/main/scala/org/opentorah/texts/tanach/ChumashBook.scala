package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.podval.metadata.{HasName, Names}
import org.podval.store.{By, Pure, Store}

trait ChumashBook extends TanachBook:
  lazy val parshiot: Seq[Parsha] = Parsha.forChumash(this)

  override def storesPure: Seq[By[?]] = Seq(
    chapters.byChapter,
    new By.WithSelector[Parsha](selectorName = "parsha")
      with Pure.With[Parsha](storesPure = parshiot)
  )

  // Parsed names of the book are ignored - names of the first parsha are used instead.
  override def names: Names = parshiot.head.names

  override def parse(names: Names, chapters: Chapters, dto: BookDto): ChumashBook.Parsed =
    ChumashBook.parse(this, names, chapters, dto)

  def metadata: ChumashBook.Metadata = TanachBook.metadata(this).asInstanceOf[ChumashBook.Metadata]

object ChumashBook:
  def parse(book: ChumashBook, names: Names, chapters: Chapters, dto: BookDto): Parsed =
    require(dto.days.isEmpty && dto.weekDays.isEmpty && dto.books.isEmpty)
    val weeks: Seq[Parsha.Parsed] = dto.weeks.map(Parsha.decode(book, _))
    require(names.getDefaultName.isDefined, "Only default name is allowed for a Chumash book")
    require(weeks.head.names.hasName(names.getDefaultName.get),
      "Chumash book name must be a name of the book's first parsha")
    Parsed(book, names, chapters, weeks)
  
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

    def resolve: Metadata =
      val parsha2metadataParsed: Map[Parsha, Parsha.Parsed] = HasName.bind[Parsha, Parsha.Parsed](
        keys = book.parshiot,
        metadatas = weeks,
        getKey = _.parsha
      )

      val parsha2span: Map[Parsha, Span] = Collections.inSequence(
        keys = book.parshiot,
        map = Collections.mapValues(parsha2metadataParsed)(_.span),
        f = (pairs: Seq[(Parsha, SpanSemiResolved)]) =>
          SpanSemiResolved.setImpliedTo(pairs.map(_._2), chapters.full, chapters)
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
      Metadata(book, parsha2metadata)

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
          Some(Torah.processDays(
            book,
            combined,
            book.chapters.merge(
              parsha2span(parsha),
              parsha2span(parshaNext)
            )
          ))

        result +: combineDays(parsha2span, (parshaNext, daysNext) +: weeks.tail.tail)
