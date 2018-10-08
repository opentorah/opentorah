package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Holder, Metadata, Named, NamedCompanion, Names, XML}

import scala.xml.Elem

sealed class SpecialDay(override val name: String) extends Named {
  final override def names: Names = SpecialDay.toNames(this)
}

object SpecialDay extends NamedCompanion {

  override type Key = SpecialDay

  case object ShabbosErevRoshChodesh extends SpecialDay("Shabbos Erev Rosh Chodesh")
  case object RoshChodesh extends SpecialDay("Rosh Chodesh")
  case object ShabbosRoshChodeshAdditionalHaftorah extends SpecialDay("Shabbos Rosh Chodesh Additional Haftorah")
  case object ShabbosErevRoshChodeshAdditionalHaftorah extends SpecialDay("Shabbos Erev Rosh Chodesh Additional Haftorah")
  case object PublicFast extends SpecialDay("Public Fast")
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
  case object ChannukahKorbanot extends SpecialDay("Channukah Korbanot")
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

  override val values: Seq[SpecialDay] = Seq(
    ShabbosErevRoshChodesh, ShabbosErevRoshChodeshAdditionalHaftorah,
    RoshChodesh, ShabbosRoshChodeshAdditionalHaftorah,
    PublicFast, TishaBeAv, ShabbosCholHamoedTorah,
    RoshHashanahMaftir, RoshHashanah1, RoshHashanah2, YomKippurShacharis, YomKippurMincha,
    SuccosMaftir, Succos1_2, Succos1, Succos2, SuccosKorbanot, SuccosShabbosCholHamoedHaftarah,
    SheminiAtzeresMaftir, SheminiAtzeres, SimchasTorah,
    ChannukahKorbanot, ChannukahShabbos1, ChannukahShabbos2,
    ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh,
    Purim, ShabbosHagodol,
    Pesach1Maftir, PesachCholHamoedMaftir, PesachAdditionalTorahReading, PesachShabbosCholHamoedHaftarah,
    Pesach1, Pesach2, Pesach3, Pesach4, Pesach5, Pesach6, Pesach7, Pesach8,
    ShavuosMaftir, Shavuos1, Shavuos2
  )

  override lazy val toNames: Map[SpecialDay, Names] = metadatas.names

  private final case class SpecialDayMetadata(
    names: Names,
    torah: Option[Elem],
    maftir: Option[Elem],
    haftarah: Option[Custom.Of[Seq[ProphetSpan.BookSpan]]]
  )

  private object metadatas extends Holder[SpecialDay, SpecialDayMetadata] {
    protected override def load: Map[SpecialDay, SpecialDayMetadata] = Metadata.loadMetadata(
      keys = values,
      obj = SpecialDay.this,
      elementName = "day"
    ).map { case (day, metadata) =>
      metadata.attributes.close()

      println(metadata.names)

      val (torahElements, maftriElements, haftarahElements) = XML.span(metadata.elements,
        "torah", "maftir", "haftarah")

      def noMoreThanOne(elements: Seq[Elem]): Option[Elem] = {
        require(elements.length <= 1)
        elements.headOption
      }

      day -> SpecialDayMetadata(
        metadata.names,
        torah = noMoreThanOne(torahElements),
        maftir = noMoreThanOne(maftriElements),
        haftarah = noMoreThanOne(haftarahElements).map(parseHaftarah)
      )
    }

    private def parseHaftarah(element: Elem): Custom.Of[Seq[ProphetSpan.BookSpan]] = {
      val (attributes, elements) = XML.open(element, "haftarah")
      Haftarah.parse(attributes, elements)
    }

    override def names: Map[SpecialDay, Names] = get.mapValues(_.names)
  }

  def main(args: Array[String]): Unit = {
    print(SpecialDay.toNames)
  }
}
