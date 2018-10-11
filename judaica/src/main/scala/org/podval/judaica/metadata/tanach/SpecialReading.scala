package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, Named, Names, Metadata, XML, Language, LanguageSpec}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

import scala.xml.Elem

sealed class SpecialReading(override val name: String) extends Named {
  final override def names: Names = SpecialReading.metadatas(this).names

  final def weekdayAliyot: Option[Aliyot] = SpecialReading.metadatas(this).weekdayAliyot

  final def shabbosAliyot: Option[Aliyot] = SpecialReading.metadatas(this).shabbosAliyot

  final def maftir: Option[ChumashSpan.BookSpan] = SpecialReading.metadatas(this).maftir

  final def haftarah: Option[Haftarah] = SpecialReading.metadatas(this).haftarah

  // TODO verify: aliyot present when they should etc...
}

object SpecialReading {

  case object ShabbosErevRoshChodesh extends SpecialReading("Shabbos Erev Rosh Chodesh")
  case object RoshChodesh extends SpecialReading("Rosh Chodesh")
  case object ShabbosRoshChodeshAdditionalHaftorah extends SpecialReading("Shabbos Rosh Chodesh Additional Haftorah")
  case object ShabbosErevRoshChodeshAdditionalHaftorah extends SpecialReading("Shabbos Erev Rosh Chodesh Additional Haftorah")
  case object PublicFastTorahPart1 extends SpecialReading("Public Fast Torah Part 1")
  case object PublicFastTorahPart2 extends SpecialReading("Public Fast Torah Part 2")
  case object PublicFastHaftarah extends SpecialReading("Public Fast Haftarah")
  case object TishaBeAv extends SpecialReading("Tisha Bav")
  case object ShabbosCholHamoedTorah extends SpecialReading("Shabbos Chol Hamoed Torah")
  case object RoshHashanahMaftir extends SpecialReading("Rosh Hashanah Maftir")
  case object RoshHashanah1 extends SpecialReading("Rosh Hashanah 1")
  case object RoshHashanah2 extends SpecialReading("Rosh Hashanah 2")
  case object YomKippurShacharis extends SpecialReading("Yom Kippur Shacharis")
  case object YomKippurMincha extends SpecialReading("Yom Kippur Mincha")
  case object SuccosMaftir extends SpecialReading("Succos Maftir")
  case object Succos1_2 extends SpecialReading("Succos 1-2")
  case object Succos1 extends SpecialReading("Succos 1")
  case object Succos2 extends SpecialReading("Succos 2")
  case object SuccosKorbanot extends SpecialReading("Succos Korbanot")
  case object SuccosShabbosCholHamoedHaftarah extends SpecialReading("Succos Shabbos Chol Hamoed Haftarah")
  case object SheminiAtzeresMaftir extends SpecialReading("Shemini Atzeres Maftir")
  case object SheminiAtzeres extends SpecialReading("Shemini Atzeres")
  case object SimchasTorah extends SpecialReading("Simchas Torah")
  case object SimchasTorahChassanBereishis extends SpecialReading("Simchas Torah Chassan Bereishis")
  case object ChannukahKorbanot extends SpecialReading("Channukah Korbanot")
  case object ChannukahKorbanotEnd extends SpecialReading("Channukah Korbanot End")
  case object ChannukahShabbos1 extends SpecialReading("Channukah Shabbos 1")
  case object ChannukahShabbos2 extends SpecialReading("Channukah Shabbos 2")
  case object ParshasShekalim extends SpecialReading("Parshas Shekalim")
  case object ParshasZachor extends SpecialReading("Parshas Zachor")
  case object ParshasParah extends SpecialReading("Parshas Parah")
  case object ParshasHachodesh extends SpecialReading("Parshas Hachodesh")
  case object Purim extends SpecialReading("Purim")
  case object ShabbosHagodol extends SpecialReading("Shabbos Hagodol")
  case object Pesach1Maftir extends SpecialReading("Pesach 1 Maftir")
  case object PesachCholHamoedMaftir extends SpecialReading("Pesach Chol Hamoed Maftir")
  case object PesachAdditionalTorahReading extends SpecialReading("Pesach Additional Torah Reading")
  case object Pesach1 extends SpecialReading("Pesach 1")
  case object Pesach2 extends SpecialReading("Pesach 2")
  case object Pesach2Diaspora extends SpecialReading("Pesach 2 Diaspora")
  case object Pesach3 extends SpecialReading("Pesach 3")
  case object Pesach4 extends SpecialReading("Pesach 4")
  case object Pesach5 extends SpecialReading("Pesach 5")
  case object Pesach6 extends SpecialReading("Pesach 6")
  case object PesachShabbosCholHamoedHaftarah extends SpecialReading("Pesach Shabbos Chol Hamoed Haftarah")
  case object Pesach7 extends SpecialReading("Pesach 7")
  case object Pesach8 extends SpecialReading("Pesach 8")
  case object ShavuosMaftir extends SpecialReading("Shavuos Maftir")
  case object Shavuos1 extends SpecialReading("Shavuos 1")
  case object Shavuos2 extends SpecialReading("Shavuos 2")

  val values: Seq[SpecialReading] = Seq(
    ShabbosErevRoshChodesh, ShabbosErevRoshChodeshAdditionalHaftorah,
    RoshChodesh, ShabbosRoshChodeshAdditionalHaftorah,
    PublicFastTorahPart1, PublicFastTorahPart2, PublicFastHaftarah, TishaBeAv, ShabbosCholHamoedTorah,
    RoshHashanahMaftir, RoshHashanah1, RoshHashanah2, YomKippurShacharis, YomKippurMincha,
    SuccosMaftir, Succos1_2, Succos1, Succos2, SuccosKorbanot, SuccosShabbosCholHamoedHaftarah,
    SheminiAtzeresMaftir, SheminiAtzeres, SimchasTorah, SimchasTorahChassanBereishis,
    ChannukahKorbanot, ChannukahKorbanotEnd, ChannukahShabbos1, ChannukahShabbos2,
    ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh,
    Purim, ShabbosHagodol,
    Pesach1Maftir, PesachCholHamoedMaftir, PesachAdditionalTorahReading, PesachShabbosCholHamoedHaftarah,
    Pesach1, Pesach2, Pesach2Diaspora, Pesach3, Pesach4, Pesach5, Pesach6, Pesach7, Pesach8,
    ShavuosMaftir, Shavuos1, Shavuos2
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
      Aliyot(bookSpan, result, None)
    }

    val (weekdayElements, shabbos) = XML.span(elements, "aliyah", "shabbos")
    val shabbosElements: Option[Seq[Elem]] = XML.noMoreThanOne(shabbos).map { shabbos =>
      val (shabbosAttributes, shabbosElements) = XML.open(shabbos, "shabbos")
      shabbosAttributes.close()
      XML.span(shabbosElements, "aliyah")
    }

    val weekdayAliyot = if (weekdayElements.isEmpty) None else Some(parseAliyot(weekdayElements))
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

  def main(args: Array[String]): Unit = {
    println(SpecialReading.SheminiAtzeres.shabbosAliyot.get.toString(LanguageSpec(Language.English)))
    println(SpecialReading.SheminiAtzeresMaftir.maftir.get.toString(LanguageSpec(Language.English)))
  }
}
