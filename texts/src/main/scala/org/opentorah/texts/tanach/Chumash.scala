package org.opentorah.texts.tanach

import org.opentorah.metadata.{Named, NamedCompanion, Names}
import org.opentorah.store.{By, Selector, Store, Stores}
import org.opentorah.util.{Collections, Effects}
import org.opentorah.xml.Parser

abstract class Chumash(val parshiot: Seq[Parsha]) extends Tanach.Book, NamedCompanion:
  final override type Key = Parsha

  final override def values: Seq[Parsha] = parshiot

  final class ByParsha extends By, Stores.Pure:
    override def selector: Selector = Selector.byName("parsha")
    override def storesPure: Seq[Parsha] = parshiot

  override def storesPure: Seq[Store.NonTerminal] = super.storesPure ++ Seq(new ByParsha)

  // Parsed names of the book are ignored - names of the first parsha are used instead.
  final override def names: Names = parshiot.head.names

  override def parser(names: Names, chapters: Chapters): Parser[Chumash.Parsed] = for
    weeks: Seq[Parsha.Parsed] <- Parsha.WeekParsable(this).seq()
    _ <- Effects.check(names.getDefaultName.isDefined,
      "Only default name is allowed for a Chumash book")
    _ <- Effects.check(weeks.head.names.hasName(names.getDefaultName.get),
      "Chumash book name must be a name of the book's first parsha")
  yield Chumash.Parsed(this, names, chapters, weeks)

  final override def metadata: Chumash.BookMetadata =
    Tanach.forBook(this).asInstanceOf[Chumash.BookMetadata]

object Chumash:

  final class BookMetadata(
    book: Chumash,
    parsha2metadata: Map[Parsha, Parsha.ParshaMetadata]
  ) extends Tanach.BookMetadata(book):

    def forParsha(parsha: Parsha): Parsha.ParshaMetadata = parsha2metadata(parsha)

  final class Parsed(
    book: Chumash,
    names: Names,
    chapters: Chapters,
    weeks: Seq[Parsha.Parsed]
  ) extends Tanach.Parsed(book, names, chapters):

    def resolve: Parser[BookMetadata] = for
      parsha2metadataParsed <- Named.bind[Parsha, Parsha.Parsed](
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
    yield BookMetadata(book, parsha2metadata)

    private def combineDays(
      parsha2span: Map[Parsha, Span],
      weeks: Seq[(Parsha, Custom.Sets[Seq[Torah.Numbered]])]
    ): Seq[Option[Torah.Customs]] = weeks match
      case (parsha, days) :: (parshaNext, daysNext) :: tail =>
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

        result +: combineDays(parsha2span, (parshaNext, daysNext) +: tail)

      case (parsha, _ /*days*/) :: Nil =>
        require(!parsha.combines)
        Seq(None)

      case Nil => Nil

  case object Genesis extends Chumash(Parsha.genesis)
  case object Exodus extends Chumash(Parsha.exodus)
  case object Leviticus extends Chumash(Parsha.leviticus)
  case object Numbers extends Chumash(Parsha.numbers)
  case object Deuteronomy extends Chumash(Parsha.deuteronomy)

  val all: Seq[Chumash] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  def forName(name: String): Chumash = Tanach.getForName(name).asInstanceOf[Chumash]
