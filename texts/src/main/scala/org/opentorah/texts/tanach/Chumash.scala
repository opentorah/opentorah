package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, Names}
import org.opentorah.store.{By, Selector, Store, Stores}
import org.opentorah.util.{Collections, Effects}
import org.opentorah.xml.Parser

enum Chumash extends TanachBook(nameOverride = None), HasName.Enum:
  case Genesis
  case Exodus
  case Leviticus
  case Numbers
  case Deuteronomy

  lazy val parshiot: Seq[Parsha] = Parsha.forChumash(this)

  final class ByParsha extends By[Parsha], Stores.Pure[Parsha]:
    override def selector: Selector = Selector.byName("parsha")
    override def storesPure: Seq[Parsha] = parshiot

  // TODO what is in the super?
  override def storesPure: Seq[Store.NonTerminal[Store]] = super.storesPure ++ Seq(new ByParsha)

  // Parsed names of the book are ignored - names of the first parsha are used instead.
  override def names: Names = parshiot.head.names

  override def parser(names: Names, chapters: Chapters): Parser[Chumash.Parsed] = for
    weeks: Seq[Parsha.Parsed] <- Parsha.WeekParsable(this).seq()
    _ <- Effects.check(names.getDefaultName.isDefined,
      "Only default name is allowed for a Chumash book")
    _ <- Effects.check(weeks.head.names.hasName(names.getDefaultName.get),
      "Chumash book name must be a name of the book's first parsha")
  yield Chumash.Parsed(this, names, chapters, weeks)

  def metadata: Chumash.Metadata = TanachBook.metadata(this).asInstanceOf[Chumash.Metadata]

object Chumash:
  val all: Seq[Chumash] = values.toIndexedSeq

  def forName(name: String): Chumash = TanachBook.getForName(name).asInstanceOf[Chumash]

  final class Metadata(
    book: Chumash,
    parsha2metadata: Map[Parsha, Parsha.ParshaMetadata]
  ) extends TanachBook.Metadata(book):
    def forParsha(parsha: Parsha): Parsha.ParshaMetadata = parsha2metadata(parsha)

  final class Parsed(
    book: Chumash,
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

          val book: Chumash = parsha.book
          Some(Parser.unsafeRun(Torah.processDays(
            book,
            combined,
            book.chapters.merge(
              parsha2span(parsha),
              parsha2span(parshaNext)
            )
          )))

        result +: combineDays(parsha2span, (parshaNext, daysNext) +: weeks.tail.tail)
