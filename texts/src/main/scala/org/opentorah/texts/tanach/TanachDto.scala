package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.podval.metadata.{Name, Names}
import org.podval.xml.XmlCodec
import zio.blocks.schema.{Modifier, Schema}

/** Derived XML shape of a Tanach `<book>` (and its weeks / psalm spans). */
private[tanach] final case class BookDto(
  @Modifier.config(XmlCodec.Attribute, "") n: Option[String] = None,
  @Modifier.config(XmlCodec.Element, "name") names: Seq[Name] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "chapter") chapters: Seq[ChapterDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "week") weeks: Seq[ParshaWeekDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "day") days: Seq[NumberedSpanDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "weekDay") weekDays: Seq[NumberedSpanDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "book") books: Seq[NumberedSpanDto] = Seq.empty
) derives CanEqual:
  def bookNames: Names = Names.fromDefaultName(n, names)
  def chapterLengths: Chapters =
    Collections.requireConsecutive(chapters, _.n, "chapter")
    Chapters(chapters.map(_.length))

private[tanach] object BookDto:
  given schema: Schema[BookDto] = Schema.derived
  val codec: XmlCodec[BookDto] = XmlCodec.derived

private[tanach] final case class ChapterDto(
  @Modifier.config(XmlCodec.Attribute, "") n: Int,
  @Modifier.config(XmlCodec.Attribute, "") length: Int
) derives CanEqual

private[tanach] final case class SpanDto(
  @Modifier.config(XmlCodec.Attribute, "") from: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") to: Option[String] = None
) derives CanEqual:
  def span: SpanParsed = SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to))

private[tanach] final case class NumberedSpanDto(
  @Modifier.config(XmlCodec.Attribute, "") n: Int,
  @Modifier.config(XmlCodec.Attribute, "") from: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") to: Option[String] = None
) derives CanEqual:
  def numbered: WithNumber[SpanParsed] =
    WithNumber(n, SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to)))
  def numberedSemi: Torah.Numbered =
    WithNumber(n, SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to)).semiResolve)

private[tanach] final case class DayDto(
  @Modifier.config(XmlCodec.Attribute, "") n: Int,
  @Modifier.config(XmlCodec.Attribute, "") custom: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") combined: Option[Boolean] = None,
  @Modifier.config(XmlCodec.Attribute, "") from: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") to: Option[String] = None
) derives CanEqual:
  def span: Torah.Numbered =
    WithNumber(n, SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to)).semiResolve)

private[tanach] final case class ParshaWeekDto(
  @Modifier.config(XmlCodec.Attribute, "") from: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") to: Option[String] = None,
  @Modifier.config(XmlCodec.Element, "name") names: Seq[Name] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "aliyah") aliyot: Seq[NumberedSpanDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "day") days: Seq[DayDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "maftir") maftirs: Seq[SpanDto] = Seq.empty
) derives CanEqual:
  def weekNames: Names = Names.fromDefaultName(None, names)
  def span: SpanSemiResolved =
    SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to)).semiResolve
