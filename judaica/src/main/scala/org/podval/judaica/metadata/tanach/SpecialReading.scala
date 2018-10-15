package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, Named, Names, Metadata, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

import scala.xml.Elem

sealed class SpecialReading(override val name: String) extends Named {
  final override def names: Names = SpecialReading.metadatas(this).names

  final def weekdayAliyot: Option[Aliyot] = SpecialReading.metadatas(this).weekdayAliyot

  final def shabbosAliyot: Option[Aliyot] = SpecialReading.metadatas(this).shabbosAliyot

  final def getAliyot(isShabbos: Boolean): Option[Aliyot] = if (isShabbos) shabbosAliyot else weekdayAliyot

  final def maftir: Option[ChumashSpan.BookSpan] = SpecialReading.metadatas(this).maftir

  final def haftarah: Option[Haftarah] = SpecialReading.metadatas(this).haftarah
}

object SpecialReading {

  case object ShabbosErevRoshChodesh extends SpecialReading("Shabbos Erev Rosh Chodesh")
  case object RoshChodesh extends SpecialReading("Rosh Chodesh")
  case object ShabbosRoshChodeshAdditionalHaftorah extends SpecialReading("Shabbos Rosh Chodesh Additional Haftorah")
  case object ShabbosErevRoshChodeshAdditionalHaftorah extends SpecialReading("Shabbos Erev Rosh Chodesh Additional Haftorah")
  case object PublicFastTorahPart1 extends SpecialReading("Public Fast Torah Part 1")
  case object PublicFastTorahPart2 extends SpecialReading("Public Fast Torah Part 2")
  case object TishaBeAvReading extends SpecialReading("Tisha Bav")
  case object ShabbosCholHamoedTorah extends SpecialReading("Shabbos Chol Hamoed Torah")
  case object RoshHashanah1Reading extends SpecialReading("Rosh Hashanah 1")
  case object RoshHashanah2Reading extends SpecialReading("Rosh Hashanah 2")
  case object YomKippurShacharis extends SpecialReading("Yom Kippur Shacharis")
  case object YomKippurMincha extends SpecialReading("Yom Kippur Mincha")
  case object Succos1_2 extends SpecialReading("Succos 1-2")
  case object Succos2Haftarah extends SpecialReading("Succos 2 Haftarah")
  case object SuccosKorbanot extends SpecialReading("Succos Korbanot")
  case object SuccosShabbosCholHamoedHaftarah extends SpecialReading("Succos Shabbos Chol Hamoed Haftarah")
  case object ShminiAtzeresReading extends SpecialReading("Shemini Atzeres")
  case object SimchasTorahReading extends SpecialReading("Simchas Torah")
  case object SimchasTorahChassanBereishis extends SpecialReading("Simchas Torah Chassan Bereishis")
  case object ChannukahKorbanot extends SpecialReading("Channukah Korbanot")
  case object ChannukahKorbanotEnd extends SpecialReading("Channukah Korbanot End")
  case object ChannukahShabbos1 extends SpecialReading("Channukah Shabbos 1")
  case object ChannukahShabbos2 extends SpecialReading("Channukah Shabbos 2")
  case object ParshasShekalim extends SpecialReading("Parshas Shekalim") // TODO in Schedule
  case object ParshasZachor extends SpecialReading("Parshas Zachor") // TODO in Schedule
  case object ParshasParah extends SpecialReading("Parshas Parah") // TODO in Schedule
  case object ParshasHachodesh extends SpecialReading("Parshas Hachodesh") // TODO in Schedule
  case object PurimReading extends SpecialReading("Purim")
  case object ShabbosHagodol extends SpecialReading("Shabbos Hagodol") // TODO in Schedule
  case object PesachCholHamoedMaftir extends SpecialReading("Pesach Chol Hamoed Maftir")
  case object PesachAdditionalTorahReading extends SpecialReading("Pesach Additional Torah Reading")
  case object Pesach1Reading extends SpecialReading("Pesach 1")
  case object Pesach2Reading extends SpecialReading("Pesach 2") // TODO rename "InHolyLand"
  case object Pesach2DiasporaReading extends SpecialReading("Pesach 2 Diaspora") // TODO rename Pesach 2
  case object Pesach3Reading extends SpecialReading("Pesach 3")
  case object Pesach4Reading extends SpecialReading("Pesach 4")
  case object Pesach5Reading extends SpecialReading("Pesach 5")
  case object Pesach6Reading extends SpecialReading("Pesach 6")
  case object PesachShabbosCholHamoedHaftarah extends SpecialReading("Pesach Shabbos Chol Hamoed Haftarah")
  case object Pesach7Reading extends SpecialReading("Pesach 7")
  case object Pesach8Reading extends SpecialReading("Pesach 8")
  case object Shavuos1Reading extends SpecialReading("Shavuos 1")
  case object Shavuos2Haftarah extends SpecialReading("Shavuos 2 Haftarah")

  val values: Seq[SpecialReading] = Seq(
    ShabbosErevRoshChodesh, ShabbosErevRoshChodeshAdditionalHaftorah,
    RoshChodesh, ShabbosRoshChodeshAdditionalHaftorah,
    PublicFastTorahPart1, PublicFastTorahPart2, TishaBeAvReading, ShabbosCholHamoedTorah,
    RoshHashanah1Reading, RoshHashanah2Reading, YomKippurShacharis, YomKippurMincha,
    Succos1_2, Succos2Haftarah, SuccosKorbanot, SuccosShabbosCholHamoedHaftarah,
    ShminiAtzeresReading, SimchasTorahReading, SimchasTorahChassanBereishis,
    ChannukahKorbanot, ChannukahKorbanotEnd, ChannukahShabbos1, ChannukahShabbos2,
    ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh,
    PurimReading, ShabbosHagodol,
    PesachCholHamoedMaftir, PesachAdditionalTorahReading, PesachShabbosCholHamoedHaftarah,
    Pesach1Reading, Pesach2Reading, Pesach2DiasporaReading, Pesach3Reading,
    Pesach4Reading, Pesach5Reading, Pesach6Reading, Pesach7Reading, Pesach8Reading,
    Shavuos1Reading, Shavuos2Haftarah
  )

  private final case class SpecialReadingMetadata(
    names: Names,
    weekdayAliyot: Option[Aliyot],
    shabbosAliyot: Option[Aliyot],
    maftir: Option[ChumashSpan.BookSpan],
    haftarah: Option[Haftarah]
  )

  private lazy val metadatas: Map[SpecialReading, SpecialReadingMetadata] = Metadata.loadMetadata(
    keys = values,
    obj = SpecialReading.this,
    elementName = "day"
  ).map { case (day, metadata) =>
    metadata.attributes.close()

    val (torahElements, maftriElements, haftarahElements) = XML.span(metadata.elements,
      "torah", "maftir", "haftarah")

    val (weekdayAliyot: Option[Aliyot], shabbosAliyot: Option[Aliyot]) =
      XML.noMoreThanOne(torahElements).fold[(Option[Aliyot], Option[Aliyot])]((None, None))(parseTorah)

    day -> SpecialReadingMetadata(
      metadata.names,
      weekdayAliyot = weekdayAliyot,
      shabbosAliyot = shabbosAliyot,
      maftir = XML.noMoreThanOne(maftriElements).map(parseMaftir),
      haftarah = XML.noMoreThanOne(haftarahElements).map(parseHaftarah)
    )
  }

  private def parseTorah(element: Elem): (Option[Aliyot], Option[Aliyot]) = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: BookSpan.ChumashSpan.BookSpan = BookSpan.ChumashSpan.parse(attributes).resolve
    attributes.close()

    def parseAliyot(elements: Seq[Elem]): Aliyot = {
      val fromChapter: Int = bookSpan.span.from.chapter
      val result = elements.map(element => XML.parseEmpty(element, "aliyah", parseNumbered(fromChapter)))
      Aliyot(bookSpan, result, number = None)
    }

    val (weekdayElements, shabbos) = XML.span(elements, "aliyah", "shabbos")
    val shabbosElements: Option[Seq[Elem]] = XML.noMoreThanOne(shabbos).map { shabbos =>
      val (shabbosAttributes, shabbosElements) = XML.open(shabbos, "shabbos")
      shabbosAttributes.close()
      XML.span(shabbosElements, "aliyah")
    }

    val weekdayAliyot =
      if (weekdayElements.nonEmpty) Some(parseAliyot(weekdayElements))
      else if (shabbosElements.isEmpty) Some(Aliyot(bookSpan, Seq(bookSpan.span)))
      else None

    val shabbosAliyot = shabbosElements.map(elements => parseAliyot(elements))

    shabbosAliyot.foreach(aliyot => require(aliyot.aliyot.length == 7))

    (weekdayAliyot, shabbosAliyot)
  }

  private def parseNumbered(fromChapter: Int)(attributes: Attributes): Span.Numbered = {
    val span: Span.SemiResolved = Span.parse(attributes).defaultFromChapter(fromChapter).semiResolve
    require(span.to.isEmpty)
    Span.Numbered(
      n = attributes.doGetInt("n"),
      span = span
    )
  }

  private def parseMaftir(element: Elem): ChumashSpan.BookSpan =
    XML.parseEmpty(element, "maftir", ChumashSpan.parse).resolve

  private def parseHaftarah(element: Elem): Haftarah = {
    val (attributes, elements) = XML.open(element, "haftarah")
    Haftarah(attributes, elements)
  }
}
