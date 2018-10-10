package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, Holder, Named, Names, Metadata, XML, Language, LanguageSpec}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

import scala.xml.Elem

sealed class SpecialDay(override val name: String) extends Named {
  final override def names: Names = SpecialDay.toNames(this)

  final def weekdayAliyot: Option[Aliyot] = SpecialDay.toWeekdayAliyot(this)

  final def shabbosAliyot: Option[Aliyot] = SpecialDay.toShabbosAliyot(this)

  final def maftir: Option[ChumashSpan.BookSpan] = SpecialDay.toMaftir(this)

  final def haftarah: Option[Haftarah] = SpecialDay.toHaftarah(this)

  // TODO verify: aliyot present when they should etc...
}

object SpecialDay {

  case object ShabbosErevRoshChodesh extends SpecialDay("Shabbos Erev Rosh Chodesh")
  case object RoshChodesh extends SpecialDay("Rosh Chodesh")
  case object ShabbosRoshChodeshAdditionalHaftorah extends SpecialDay("Shabbos Rosh Chodesh Additional Haftorah")
  case object ShabbosErevRoshChodeshAdditionalHaftorah extends SpecialDay("Shabbos Erev Rosh Chodesh Additional Haftorah")
  case object PublicFastTorahPart1 extends SpecialDay("Public Fast Torah Part 1")
  case object PublicFastTorahPart2 extends SpecialDay("Public Fast Torah Part 2")
  case object PublicFastHaftarah extends SpecialDay("Public Fast Haftarah")
  case object TishaBeAv extends SpecialDay("Tisha Bav")
  case object ShabbosCholHamoedTorah extends SpecialDay("Shabbos Chol Hamoed Torah")
  case object RoshHashanahMaftir extends SpecialDay("Rosh Hashanah Maftir")
  case object RoshHashanah1 extends SpecialDay("Rosh Hashanah 1")
  case object RoshHashanah2 extends SpecialDay("Rosh Hashanah 2")
  case object YomKippurShacharis extends SpecialDay("Yom Kippur Shacharis")
  case object YomKippurMincha extends SpecialDay("Yom Kippur Mincha")
  case object SuccosMaftir extends SpecialDay("Succos Maftir")
  case object Succos1_2 extends SpecialDay("Succos 1-2")
  case object Succos1 extends SpecialDay("Succos 1")
  case object Succos2 extends SpecialDay("Succos 2")
  case object SuccosKorbanot extends SpecialDay("Succos Korbanot")
  case object SuccosShabbosCholHamoedHaftarah extends SpecialDay("Succos Shabbos Chol Hamoed Haftarah")
  case object SheminiAtzeresMaftir extends SpecialDay("Shemini Atzeres Maftir")
  case object SheminiAtzeres extends SpecialDay("Shemini Atzeres")
  case object SimchasTorah extends SpecialDay("Simchas Torah")
  case object SimchasTorahChassanBereishis extends SpecialDay("Simchas Torah Chassan Bereishis")
  case object ChannukahKorbanot extends SpecialDay("Channukah Korbanot")
  case object ChannukahKorbanotEnd extends SpecialDay("Channukah Korbanot End")
  case object ChannukahShabbos1 extends SpecialDay("Channukah Shabbos 1")
  case object ChannukahShabbos2 extends SpecialDay("Channukah Shabbos 2")
  case object ParshasShekalim extends SpecialDay("Parshas Shekalim")
  case object ParshasZachor extends SpecialDay("Parshas Zachor")
  case object ParshasParah extends SpecialDay("Parshas Parah")
  case object ParshasHachodesh extends SpecialDay("Parshas Hachodesh")
  case object Purim extends SpecialDay("Purim")
  case object ShabbosHagodol extends SpecialDay("Shabbos Hagodol")
  case object Pesach1Maftir extends SpecialDay("Pesach 1 Maftir")
  case object PesachCholHamoedMaftir extends SpecialDay("Pesach Chol Hamoed Maftir")
  case object PesachAdditionalTorahReading extends SpecialDay("Pesach Additional Torah Reading")
  case object Pesach1 extends SpecialDay("Pesach 1")
  case object Pesach2 extends SpecialDay("Pesach 2")
  case object Pesach2Diaspora extends SpecialDay("Pesach 2 Diaspora")
  case object Pesach3 extends SpecialDay("Pesach 3")
  case object Pesach4 extends SpecialDay("Pesach 4")
  case object Pesach5 extends SpecialDay("Pesach 5")
  case object Pesach6 extends SpecialDay("Pesach 6")
  case object PesachShabbosCholHamoedHaftarah extends SpecialDay("Pesach Shabbos Chol Hamoed Haftarah")
  case object Pesach7 extends SpecialDay("Pesach 7")
  case object Pesach8 extends SpecialDay("Pesach 8")
  case object ShavuosMaftir extends SpecialDay("Shavuos Maftir")
  case object Shavuos1 extends SpecialDay("Shavuos 1")
  case object Shavuos2 extends SpecialDay("Shavuos 2")

  val values: Seq[SpecialDay] = Seq(
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

  private lazy val toNames: Map[SpecialDay, Names] = metadatas.names

  private lazy val toWeekdayAliyot: Map[SpecialDay, Option[Aliyot]] = metadatas.weekdayAliyot

  private lazy val toShabbosAliyot: Map[SpecialDay, Option[Aliyot]] = metadatas.shabbosAliyot

  private lazy val toMaftir: Map[SpecialDay, Option[ChumashSpan.BookSpan]] = metadatas.maftir

  private lazy val toHaftarah: Map[SpecialDay, Option[Haftarah]] = metadatas.haftarah

  private final case class SpecialDayMetadata(
    names: Names,
    weekdayAliyot: Option[Aliyot],
    shabbosAliyot: Option[Aliyot],
    maftir: Option[ChumashSpan.BookSpan],
    haftarah: Option[Haftarah]
  )

  private object metadatas extends Holder[SpecialDay, SpecialDayMetadata] {
    protected override def load: Map[SpecialDay, SpecialDayMetadata] = Metadata.loadMetadata(
      keys = values,
      obj = SpecialDay.this,
      elementName = "day"
    ).map { case (day, metadata) =>
      metadata.attributes.close()

      val (torahElements, maftriElements, haftarahElements) = XML.span(metadata.elements,
        "torah", "maftir", "haftarah")

      val (weekdayAliyot: Option[Aliyot], shabbosAliyot: Option[Aliyot]) =
        XML.noMoreThanOne(torahElements).fold[(Option[Aliyot], Option[Aliyot])]((None, None))(parseTorah)

      day -> SpecialDayMetadata(
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
        Aliyot.parseAliyot(bookSpan, result)
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

    override def names: Map[SpecialDay, Names] = get.mapValues(_.names)

    def weekdayAliyot: Map[SpecialDay, Option[Aliyot]] = get.mapValues(_.weekdayAliyot)

    def shabbosAliyot: Map[SpecialDay, Option[Aliyot]] = get.mapValues(_.shabbosAliyot)

    def maftir: Map[SpecialDay, Option[ChumashSpan.BookSpan]] = get.mapValues(_.maftir)

    def haftarah: Map[SpecialDay, Option[Haftarah]] = get.mapValues(_.haftarah)
  }

  def main(args: Array[String]): Unit = {
    println(SpecialDay.SheminiAtzeres.shabbosAliyot.get.toString(LanguageSpec(Language.English)))
    println(SpecialDay.SheminiAtzeresMaftir.maftir.get.toString(LanguageSpec(Language.English)))
  }
}
