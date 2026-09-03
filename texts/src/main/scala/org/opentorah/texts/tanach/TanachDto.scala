package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.podval.metadata.{Name, Names}
import org.podval.xml.XmlCodec
import zio.blocks.schema.{Modifier, Schema}

/** Derived XML shape of a Tanach `<book>` (and its weeks / psalm spans). */
private[tanach] final case class BookDto(
  @Modifier.config(XmlCodec.Attribute, "") n: Option[String] = None,
  @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "chapter") chapters: Seq[ChapterDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "week") weeks: Seq[ParshaWeekDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "day") days: Seq[NumberedSpanDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "weekDay") weekDays: Seq[NumberedSpanDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "book") books: Seq[NumberedSpanDto] = Seq.empty
) derives CanEqual:
  def bookNames: Names = Names.fromDefaultName(n, names.map(Name.fromData))
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
  @Modifier.config(XmlCodec.Attribute, "") fromChapter: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") fromVerse: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") toChapter: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") toVerse: Option[Int] = None
) derives CanEqual:
  def span: SpanParsed = SpanParsed(VerseParsed(fromChapter, fromVerse), VerseParsed(toChapter, toVerse))

private[tanach] final case class NumberedSpanDto(
  @Modifier.config(XmlCodec.Attribute, "") n: Int,
  @Modifier.config(XmlCodec.Attribute, "") fromChapter: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") fromVerse: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") toChapter: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") toVerse: Option[Int] = None
) derives CanEqual:
  def numbered: WithNumber[SpanParsed] =
    WithNumber(n, SpanParsed(VerseParsed(fromChapter, fromVerse), VerseParsed(toChapter, toVerse)))
  def numberedSemi: Torah.Numbered =
    WithNumber(n, SpanParsed(VerseParsed(fromChapter, fromVerse), VerseParsed(toChapter, toVerse)).semiResolve)

private[tanach] final case class DayDto(
  @Modifier.config(XmlCodec.Attribute, "") n: Int,
  @Modifier.config(XmlCodec.Attribute, "") custom: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") combined: Option[Boolean] = None,
  @Modifier.config(XmlCodec.Attribute, "") fromChapter: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") fromVerse: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") toChapter: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") toVerse: Option[Int] = None
) derives CanEqual:
  def span: Torah.Numbered =
    WithNumber(n, SpanParsed(VerseParsed(fromChapter, fromVerse), VerseParsed(toChapter, toVerse)).semiResolve)

private[tanach] final case class ParshaWeekDto(
  @Modifier.config(XmlCodec.Attribute, "") fromChapter: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") fromVerse: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") toChapter: Option[Int] = None,
  @Modifier.config(XmlCodec.Attribute, "") toVerse: Option[Int] = None,
  @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "aliyah") aliyot: Seq[NumberedSpanDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "day") days: Seq[DayDto] = Seq.empty,
  @Modifier.config(XmlCodec.Element, "maftir") maftirs: Seq[SpanDto] = Seq.empty
) derives CanEqual:
  def weekNames: Names = Names.fromDefaultName(None, names.map(Name.fromData))
  def span: SpanSemiResolved =
    SpanParsed(VerseParsed(fromChapter, fromVerse), VerseParsed(toChapter, toVerse)).semiResolve
