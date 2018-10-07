package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.tanach.SpanParser.{NumberedSpan, SpanParsed}
import org.podval.judaica.metadata.{Holder, Metadata, Named, Names, XML}

import scala.xml.Elem

object Tanach extends Named {

  override type Key = TanachBook

  sealed trait TanachBook extends Named.NamedBase {
    final def chapters: Chapters = toChapters(this)
  }

  override lazy val toNames: Map[TanachBook, Names] =
    metadata.get.mapValues(_.names)

  private lazy val toChapters: Map[TanachBook, Chapters] =
    metadata.get.mapValues(metadata => Chapters(metadata.chapterElements))

  sealed abstract class ChumashBook(val parshiot: Seq[Parsha]) extends TanachBook {
    private val metadata = new Holder[Map[Parsha, ParshaMetadata]] {
      protected override def load: Map[Parsha, ParshaMetadata] =
        Metadata.bind(
          parshiot,
          Tanach.metadata.get(ChumashBook.this).weekElements
            .map(element => Metadata.loadSubresource(this, element, "week"))
        ).mapValues { value =>

          def byCustom(days: Seq[Tanach.DayParsed]): Custom.Sets[Seq[NumberedSpan]] =
            days.groupBy(_.custom).mapValues(days => days.map(_.span))

          val span = SpanParser.parseSpan(value.attributes)
          value.attributes.close()

          val (aliyahElements, dayElements, maftirElements) = XML.span(value.elements,
            "aliyah", "day", "maftir")
          require(maftirElements.length == 1)

          val (days: Seq[DayParsed], daysCombined: Seq[DayParsed]) = dayElements.map(parseDay).partition(!_.isCombined)

          ParshaMetadata(
            names = value.names,
            span = span,
            days = byCustom(days),
            daysCombined = byCustom(daysCombined),
            aliyahElements = aliyahElements,
            maftirElements = maftirElements
          )
        }
    }

    final override def names: Names = toNames(parshiot.head)

    lazy val toNames: Map[Parsha, Names] = metadata.get.mapValues(_.names)

    lazy val span: Map[Parsha, Span] = Util.inSequence(
      keys = parshiot,
      map = metadata.get.mapValues(_.span),
      f = (pairs: Seq[(Parsha, SpanParsed)]) => SpanParser.setImpliedTo(pairs.map(_._2), chapters.full, chapters)
    )

    lazy val days: Map[Parsha, Custom.Of[Seq[Span]]] = metadata.get.map { case (parsha, value) =>
      parsha -> Custom.denormalize(processDays(value.days, parsha.span, chapters))
    }

    lazy val aliyot: Map[Parsha, Seq[Span]] = metadata.get.map { case (parsha, value) =>
      val aliyot: Seq[NumberedSpan] = value.aliyahElements.map(element => SpanParser.parseNumberedSpan(element, "aliyah"))
      // TODO QUESTION if Cohen ends in a custom place, does it affect the end of the 3 aliyah on Mon/Thu?
      // TODO QUESTION if the parshiot combine, does it affect those small aliyot?
      val aliyotSpan: Span = Span(parsha.span.from, aliyot.last.span.to.getOrElse(parsha.days(Custom.Common).head.to))
      val aliyotWithImplied1: Seq[NumberedSpan] = SpanParser.addImplied1(aliyot, aliyotSpan, chapters)
      val result: Seq[Span] = SpanParser.setImpliedToCheckAndDropNumbers(aliyotWithImplied1, 3, aliyotSpan, chapters)
      parsha -> result
    }

    lazy val maftir: Map[Parsha, Span] = metadata.get.map { case (parsha, value) =>
      val maftir: SpanParsed = SpanParser.parseSpan(value.maftirElements.head, "maftir")
      val result: Span = SpanParser.setImpliedTo(
        Seq(maftir),
        Span(maftir.from, maftir.to.getOrElse(parsha.span.to)),
        chapters
      ).head
      parsha -> result
    }

    lazy val daysCombined: Map[Parsha, Custom.Of[Seq[Span]]] = Util.inSequence(
      keys = parshiot,
      map = metadata.get.map { case (parsha: Parsha, metadata: Tanach.ParshaMetadata) => parsha -> metadata.daysCombined },
      f = combine
    )
  }

  private def combine(weeks: Seq[(Parsha, Custom.Sets[Seq[NumberedSpan]])]): Seq[Custom.Of[Seq[Span]]] = weeks match {
    case (parsha1, days1) :: (parsha2, days2) :: tail =>
      combine(parsha1, days1, days2, parsha2.span) +: combine((parsha2, days2) +: tail)
    case (parsha, days) :: Nil =>
      Seq(combine(parsha, days, Map.empty, Span(Verse(1, 1), Verse(1, 1)))) // The Span will never be used!
    case Nil => Nil
  }

  private def combine(
    parsha: Parsha,
    daysCombined: Custom.Sets[Seq[NumberedSpan]],
    daysCombinedNext: Custom.Sets[Seq[NumberedSpan]],
    spanNext: Span
  ): Custom.Of[Seq[Span]] = {
    val chapters: Chapters = parsha.book.chapters

    def combine: Custom.Sets[Seq[Span]] = {
      // TODO Use defaults from days?
      val combinedFull = daysCombinedNext ++ daysCombined.map { case (customs, value) =>
        (customs, value ++ daysCombinedNext.getOrElse(customs, Seq.empty))
      }
      processDays(combinedFull, chapters.merge(parsha.span, spanNext), chapters)
    }

    if (!parsha.combines) Map.empty else Custom.denormalize(combine)
  }

  private def processDays(
    days: Custom.Sets[Seq[NumberedSpan]],
    span: Span,
    chapters: Chapters
  ): Custom.Sets[Seq[Span]] = {
    val withImplied1 = SpanParser.addImplied1(Custom.common(days), span, chapters)

    days.mapValues { spans: Seq[NumberedSpan] =>
      val overlayedSpans = SpanParser.overlaySpans(withImplied1, spans)
      SpanParser.setImpliedToCheckAndDropNumbers(overlayedSpans, 7, span, chapters)
    }
  }

  case object Genesis extends ChumashBook(Parsha.genesis)
  case object Exodus extends ChumashBook(Parsha.exodus)
  case object Leviticus extends ChumashBook(Parsha.leviticus)
  case object Numbers extends ChumashBook(Parsha.numbers)
  case object Deuteronomy extends ChumashBook(Parsha.deuteronomy)

  val chumash: Seq[ChumashBook] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  sealed trait NachBook extends TanachBook {
    final override def names: Names = toNames(this)
  }

  sealed trait ProphetsBook extends NachBook

  sealed trait EarlyProphetsBook extends ProphetsBook

  case object Joshua extends EarlyProphetsBook
  case object Judges extends EarlyProphetsBook
  case object SamuelI extends EarlyProphetsBook { override def name: String = "I Samuel" }
  case object SamuelII extends EarlyProphetsBook { override def name: String = "II Samuel" }
  case object KingsI extends EarlyProphetsBook { override def name: String = "I Kings" }
  case object KingsII extends EarlyProphetsBook { override def name: String = "II Kings" }

  val earlyProphets: Seq[ProphetsBook] = Seq(Joshua, Judges, SamuelI, SamuelII, KingsI, KingsII)

  sealed trait LateProphetsBook extends ProphetsBook

  case object Isaiah extends LateProphetsBook
  case object Jeremiah extends LateProphetsBook
  case object Ezekiel extends LateProphetsBook

  <!-- תרי עשר -->
  sealed trait TreiAsarBook extends LateProphetsBook

  case object Hosea extends TreiAsarBook
  case object Joel extends TreiAsarBook
  case object Amos extends TreiAsarBook
  case object Obadiah extends TreiAsarBook
  case object Jonah extends TreiAsarBook
  case object Micah extends TreiAsarBook
  case object Nahum extends TreiAsarBook
  case object Habakkuk extends TreiAsarBook
  case object Zephaniah extends TreiAsarBook
  case object Haggai extends TreiAsarBook
  case object Zechariah extends TreiAsarBook
  case object Malachi extends TreiAsarBook

  val treiAsar: Seq[TreiAsarBook] = Seq(Hosea, Joel, Amos, Obadiah, Jonah, Micah,
    Nahum, Habakkuk, Zephaniah, Haggai, Zechariah, Malachi)

  val lateProphets: Seq[ProphetsBook] = Seq(Isaiah, Jeremiah, Ezekiel) ++ treiAsar

  val prophets: Seq[ProphetsBook] = earlyProphets ++ lateProphets

  def getProhetForName(name: String): ProphetsBook = getForName(name).asInstanceOf[ProphetsBook]

  sealed trait WritingsBook extends NachBook

  case object Psalms extends WritingsBook
  case object Proverbs extends WritingsBook
  case object Job extends WritingsBook
  case object SongOfSongs extends WritingsBook { override def name: String = "Song of Songs" }
  case object Ruth extends WritingsBook
  case object Lamentations extends WritingsBook
  case object Ecclesiastes extends WritingsBook
  case object Esther extends WritingsBook
  case object Daniel extends WritingsBook
  case object Ezra extends WritingsBook
  case object Nehemiah extends WritingsBook
  case object ChroniclesI extends WritingsBook { override def name: String = "I Chronicles" }
  case object ChroniclesII extends WritingsBook { override def name: String = "II Chronicles" }

  val writings: Seq[WritingsBook] = Seq(Psalms, Proverbs, Job, SongOfSongs, Ruth, Lamentations, Ecclesiastes,
    Esther, Daniel, Ezra, Nehemiah, ChroniclesI, ChroniclesII)

  val nach: Seq[TanachBook] = prophets ++ writings

  override val values: Seq[TanachBook] = chumash ++ nach

  private final case class TanachMetadata(names: Names, chapterElements: Seq[Elem], weekElements: Seq[Elem])

  private val metadata = new Holder[Map[TanachBook, TanachMetadata]] {
    protected override def load: Map[TanachBook, TanachMetadata] = Metadata.loadMetadata(
      values = values,
      obj = this,
      resourceName = "Tanach",
      rootElementName = "metadata",
      elementName = "book"
    ).map { case (book, bookMetadata) =>
      bookMetadata.attributes.close()
      val (chapterElements: Seq[Elem], weekElements: Seq[Elem]) =
        XML.span(bookMetadata.elements, "chapter", "week")
      if (!book.isInstanceOf[ChumashBook]) XML.checkNoMoreElements(weekElements)
      book -> TanachMetadata(bookMetadata.names, chapterElements, weekElements)
    }
  }

  private final case class ParshaMetadata(
    names: Names,
    span: SpanParsed,
    days: Custom.Sets[Seq[NumberedSpan]],
    daysCombined: Custom.Sets[Seq[NumberedSpan]],
    aliyahElements: Seq[Elem],
    maftirElements: Seq[Elem]
  )

  private final class DayParsed(
    val span: NumberedSpan,
    val custom: Set[Custom],
    val isCombined: Boolean
  )

  private def parseDay(element: Elem): DayParsed = {
    val attributes = XML.openEmpty(element, "day")
    val result = new DayParsed(
      span = SpanParser.parseNumberedSpan(attributes),
      custom = attributes.get("custom").fold[Set[Custom]](Set(Custom.Common))(Custom.parse),
      isCombined = attributes.doGetBoolean("combined")
    )
    attributes.close()
    result
  }
}
