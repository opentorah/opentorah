package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.podval.metadata.{Name, Names}
import org.podval.xml.XmlCodec
import zio.blocks.schema.{Modifier, Schema}

/** Derived XML shape of a Tanach `<book>` (and its weeks / psalm spans). */
private[tanach] final case class BookDto(
  n: Option[String] = None,
  names: Seq[Name] = Seq.empty,
  chapters: Seq[ChapterDto] = Seq.empty,
  weeks: Seq[ParshaWeekDto] = Seq.empty,
  @Modifier.rename("day") days: Seq[NumberedSpanDto] = Seq.empty,
  @Modifier.rename("weekDay") weekDays: Seq[NumberedSpanDto] = Seq.empty,
  @Modifier.rename("book") books: Seq[NumberedSpanDto] = Seq.empty
) derives CanEqual:
  def bookNames: Names = Names.fromDefaultName(n, names)
  def chapterLengths: Chapters =
    Collections.requireConsecutive(chapters, _.n, "chapter")
    Chapters(chapters.map(_.length))

private[tanach] object BookDto:
  given schema: Schema[BookDto] = Schema.derived
  val codec: XmlCodec[BookDto] = XmlCodec.derived

@Modifier.config(XmlCodec.Element, "chapter")
private[tanach] final case class ChapterDto(
  n: Int,
  length: Int
) derives CanEqual

private[tanach] object ChapterDto:
  given schema: Schema[ChapterDto] = Schema.derived
  val codec: XmlCodec[ChapterDto] = XmlCodec.derived

@Modifier.config(XmlCodec.Element, "maftir")
private[tanach] final case class SpanDto(
  from: Option[String] = None,
  to: Option[String] = None
) derives CanEqual:
  def span: SpanParsed = SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to))

private[tanach] object SpanDto:
  given schema: Schema[SpanDto] = Schema.derived
  val codec: XmlCodec[SpanDto] = XmlCodec.derived

private[tanach] final case class NumberedSpanDto(
  n: Int,
  from: Option[String] = None,
  to: Option[String] = None
) derives CanEqual:
  def numbered: WithNumber[SpanParsed] =
    WithNumber(n, SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to)))
  def numberedSemi: Torah.Numbered =
    WithNumber(n, SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to)).semiResolve)

@Modifier.config(XmlCodec.Element, "day")
private[tanach] final case class DayDto(
  n: Int,
  custom: Option[String] = None,
  combined: Option[Boolean] = None,
  from: Option[String] = None,
  to: Option[String] = None
) derives CanEqual:
  def span: Torah.Numbered =
    WithNumber(n, SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to)).semiResolve)

private[tanach] object DayDto:
  given schema: Schema[DayDto] = Schema.derived
  val codec: XmlCodec[DayDto] = XmlCodec.derived

@Modifier.config(XmlCodec.Element, "week")
private[tanach] final case class ParshaWeekDto(
  from: Option[String] = None,
  to: Option[String] = None,
  names: Seq[Name] = Seq.empty,
  @Modifier.rename("aliyah") aliyot: Seq[NumberedSpanDto] = Seq.empty,
  days: Seq[DayDto] = Seq.empty,
  maftirs: Seq[SpanDto] = Seq.empty
) derives CanEqual:
  def weekNames: Names = Names.fromDefaultName(None, names)
  def span: SpanSemiResolved =
    SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to)).semiResolve

private[tanach] object ParshaWeekDto:
  given schema: Schema[ParshaWeekDto] = Schema.derived
  val codec: XmlCodec[ParshaWeekDto] = XmlCodec.derived
