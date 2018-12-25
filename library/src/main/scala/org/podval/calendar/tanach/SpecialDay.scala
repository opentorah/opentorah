package org.podval.calendar.tanach

import org.podval.judaica.metadata.{Metadata, Names, WithName, WithNames}
import org.podval.judaica.tanach.{Custom, Parsha, Source, Torah}
import org.podval.judaica.tanach.Torah.{Aliyah, Maftir}
import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.calendar.jewish.Jewish.Month.Name._
import org.podval.calendar.jewish.{JewishDay, JewishYear}

import scala.xml.Elem

object SpecialDay {

  sealed class LoadNames(override val name: String) extends WithName with WithNames {
    final override def names: Names = toNames(this)
  }

  sealed trait Date extends WithNames {
    def date(year: Year): Day

    final def correctedDate(year: Year): Day = correctDate(date(year))

    protected def correctDate(date: Day): Day = date
  }

  sealed trait PostponeOnShabbos extends Date {
    final override protected def correctDate(result: Day): Day = if (result.isShabbos) result+1 else result
  }

  sealed trait DayOf extends Date {
    def firstDay: Date

    def dayNumber: Int
  }

  sealed trait FirstDayOf extends DayOf {
    final override def firstDay: Date = this

    final override def dayNumber: Int = 1
  }

  sealed trait NonFirstDayOf extends DayOf {
    final override def date(year: Year): Day = firstDay.date(year) + (dayNumber-1)
  }

  sealed trait WeekdayReading {
    def weekday: Reading
  }

  sealed trait MaftrirAndHaftarah {
    protected def maftir: Maftir

    protected def haftarah: Haftarah.Customs
  }

  sealed trait WeekdayReadingSimple extends WeekdayReading with MaftrirAndHaftarah {
    final override def weekday: Reading = Reading(torah, maftir, haftarah)

    protected def torah: Torah
  }

  sealed trait ShabbosReading {
    def shabbos: Reading
  }

  sealed trait ShabbosAndWeekdayReading extends ShabbosReading with WeekdayReading with MaftrirAndHaftarah {
    final override def weekday: Reading = getReading(weekdayTorah)

    final override def shabbos: Reading = getReading(shabbosTorah)

    private def getReading(torah: Torah): Reading = Reading(torah, maftir, haftarah)

    protected def shabbosTorah: Torah

    protected def weekdayTorah: Torah
  }

  sealed trait ShabbosAndWeekdayReadingParse extends ShabbosAndWeekdayReading with Parse { self: WithNames =>
    final override lazy val shabbosTorah: Torah = parseTorah(shabbosTorahElement)
    protected def shabbosTorahElement: Elem

    final override lazy val weekdayTorah: Torah = shabbosTorah.drop(dropOnWeekday)
    protected def dropOnWeekday: Set[Int]
  }

  sealed trait AfternoonReading {
    def afternoon: Reading
  }

  sealed trait FestivalOrIntermediate extends Date

  sealed trait Festival extends FestivalOrIntermediate with WeekdayReading

  private object FestivalEnd extends LoadNames("Festival End") with Parse {
    val endShabbosTorah: Torah = parseTorah(Deuteronomy.festivalEndShabbosTorah)
    val endWeekdayTorah: Torah = Torah(endShabbosTorah.spans.drop(2))
    val sheminiAtzeresWeekdayTorah: Torah = endShabbosTorah.drop(Set(2, 3))
  }

  private object IntermediateShabbos extends LoadNames("Intermediate Shabbos") with Parse {
    val torah: Torah = parseTorah(Exodus.intermediateShabbosTorah)
  }

  sealed abstract class Intermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends FestivalOrIntermediate with NonFirstDayOf with ShabbosReading
  {
    final override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)

    final override def shabbos: Reading = Reading(
      torah = IntermediateShabbos.torah,
      maftir = shabbosMaftir,
      haftarah = shabbosHaftarah
    )

    protected def shabbosMaftir: Maftir

    protected def shabbosHaftarah: Haftarah.Customs
  }

  sealed trait RabbinicFestival extends Date

  private def replaceMaftirAndHaftarah(
    reading: Reading,
    maftir: Maftir,
    haftarah: Haftarah.Customs
  ): Reading = reading.transform[Haftarah](haftarah, transformer = {
    case (_: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
      readingCustom.replaceMaftirAndHaftarah(maftir, haftarah)
  })

  sealed trait MaftirAndHaftarahTransform {
    protected final def transform(
      transformer: (
        Custom,
        Reading.ReadingCustom,
        Haftarah,
        Option[Haftarah]
      ) => Reading.ReadingCustom,
      reading: Reading
    ): Reading = reading.transform[(Haftarah, Option[Haftarah])](haftarahs, { case (
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarahs: (Haftarah, Option[Haftarah])
      ) =>
        val (haftarah: Haftarah, addition: Option[Haftarah]) = haftarahs
        transformer(custom, reading, haftarah, addition)
    })

    private lazy val haftarahs: Custom.Of[(Haftarah, Option[Haftarah])] =
      shabbosHaftarah * shabbosAdditionalHaftarah

    protected def shabbosHaftarah: Haftarah.Customs

    protected def shabbosAdditionalHaftarah: Haftarah.Customs
  }

  case object RoshChodesh extends LoadNames("Rosh Chodesh")
    with WeekdayReading with MaftirAndHaftarahTransform with Parse
  {
    private val torah: Torah = parseTorah(Numbers.roshChodesh)

    override def weekday: Reading = {
      val all = torah.spans
      val aliya1 = all.head+all(1)             // 1-3
      val aliya2AshkenazSefard = all(1)+all(2) // 3-5
      val aliya2Hagra = all(2)+all(3)          // 4-8
      val aliya3 = all(3)+all(4)               // 6-10
      val aliya4 = all(5)                      // 11-15
      val ashkenazSefard = Torah.aliyot(aliya1, aliya2AshkenazSefard, aliya3, aliya4)
      val hagra = Torah.aliyot(aliya1, aliya2Hagra, aliya3, aliya4)
      val torahCustoms: Torah.Customs = new Torah.Customs(Map(
        Custom.Ashkenaz -> ashkenazSefard,
        Custom.Sefard -> ashkenazSefard,
        Custom.Hagra -> hagra
      ))
      Reading(torahCustoms.map(_.fromWithNumbers(this)))
    }

    val in3aliyot: Torah = {
      val all = torah.spans
      Torah.aliyot(
        (all.head+all(1)+all(2)).from(new Source.AndNumbers(this, 1, 2)), // 1-5
        (all(3)+all(4)         ).from(new Source.AndNumber (this, 3)      ), // 6-10
        (all(5)                ).from(new Source.AndNumber (this, 4)      )  // 11-15
      )
    }

    def correct(month: Month.Name, isSpecialShabbos: Boolean, reading: Reading): Reading = {
      val allowReplace: Boolean = !isSpecialShabbos && (month != Teves) && (month != Av)

      def transformer(
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarah: Haftarah,
        addition: Option[Haftarah]
      ): Reading.ReadingCustom =
        if (allowReplace && ((month != Elul) || (custom == Custom.Chabad)))
          reading.replaceMaftirAndHaftarah(shabbosMaftir, haftarah)
        else
          reading.addHaftarah(addition)

      transform(transformer, reading)
    }

    def addShabbosMaftirAs7thAliyah(reading: Reading): Reading =
      reading.transformTorah(_.to6withLast(shabbosMaftir))

    private val shabbosMaftir: Maftir = (torah.spans(4)+torah.spans(5)).from(this)  // 9-15

    // TODO take source as a parameter; if it is always 'this', codify it in the WithHaftarah trait?
    protected override val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="66">
        <part n="1" fromVerse="1" toVerse="24"/>
        <part n="2" fromVerse="23" toVerse="23"/>
      </haftarah>).map(_.from(this))

    protected override val shabbosAdditionalHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Chabad" book="Isaiah" fromChapter="66">
          <part n="1" fromVerse="1" toVerse="1"/>
          <part n="2" fromVerse="23" toVerse="24"/>
          <part n="3" fromVerse="23" toVerse="23"/>
        </custom>
      </haftarah>).map(_.from(this), full = false)
  }

  case object ErevRoshChodesh extends LoadNames("Erev Rosh Chodesh") with MaftirAndHaftarahTransform with Parse {
    def correct(month: Month.Name, isSpecialShabbos: Boolean, isRoshChodesh: Boolean, reading: Reading): Reading = {
      val allowReplace: Boolean = specialShabbos.isEmpty && !isRoshChodesh &&
        (month != Teves) && (month != Av) && (month != Elul)

      def transformer(
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarah: Haftarah,
        addition: Option[Haftarah]
      ): Reading.ReadingCustom =
        if (allowReplace && (custom != Custom.Fes))
          reading.replaceHaftarah(haftarah)
        else
          reading.addHaftarah(addition)

      transform(transformer, reading)
    }

    protected override val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="I Samuel" fromChapter="20" fromVerse="18" toVerse="42"/>).map(_.from(this))

    protected override val shabbosAdditionalHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Chabad, Fes" book="I Samuel" fromChapter="20">
          <part n="1" fromVerse="18" toVerse="18"/>
          <part n="2" fromVerse="42" toVerse="42"/>
        </custom>
      </haftarah>).map(_.from(this), full = false)
  }

  private object Fast extends LoadNames("Public Fast") with Parse {
    val torah: Torah = Torah.aliyot(
      parseTorah(Exodus.fastAfternoonTorahPart1).spans.head, // Exodus 32:11-14
      IntermediateShabbos.torah.spans(3),                    // Exodus 34:1-3
      IntermediateShabbos.torah.spans(4)                     // Exodus 34:4-10
    ).fromWithNumbers(this)

    val defaultAfternoonHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Ashkenaz, Chabad, Morocco" book="Isaiah" fromChapter="55" fromVerse="6" toChapter="56" toVerse="8"/>
        <custom n="Algeria">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>).map(_.from(this), full = false)
  }

  sealed trait Fast extends Date with WeekdayReading with AfternoonReading {
    final override def afternoon: Reading = {
      val torah = Fast.torah.fromWithNumbers(this)
      val haftarah = afternoonHaftarah

      new Reading(
        customs = haftarah.lift { case (_: Custom, haftarah: Option[Haftarah]) =>
          haftarah.fold(Reading.ReadingCustom(torah, None)) { haftarah: Haftarah =>
            Reading.ReadingCustom(
              torah = Torah(torah.spans.init),
              maftirAndHaftarah = Some(Reading.MaftirAndHaftarah(torah.spans.last, haftarah))
            )
          }
        }.customs
      )
    }

    private def afternoonHaftarah: Haftarah.Customs =
      afternoonHaftarahExceptions.fold(Fast.defaultAfternoonHaftarah) { afternoonHaftarahExceptions =>
        Fast.defaultAfternoonHaftarah ++ afternoonHaftarahExceptions }

    protected val afternoonHaftarahExceptions: Option[Haftarah.Customs] = None
  }

  sealed trait NonTishaBeAvFast extends Fast {
    final override def weekday: Reading = Reading(Fast.torah.fromWithNumbers(this))
  }

  case object RoshHashanah1 extends LoadNames("Rosh Hashanah") with Festival with ShabbosAndWeekdayReadingParse {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(1)

    protected override def shabbosTorahElement: Elem = Genesis.roshHashana1torah
    protected override def dropOnWeekday: Set[Int] = Set(3, 5)

    override val maftir: Maftir = parseMaftir(Numbers.roshHashanahMaftir)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="I Samuel" fromChapter="1" fromVerse="1" toChapter="2" toVerse="10"/>).map(_.from(this))
  }

  case object RoshHashanah2 extends Festival with NonFirstDayOf with WeekdayReadingSimple with Parse {
    override def firstDay: Date = RoshHashanah1

    override def dayNumber: Int = 2

    override lazy val names: Names = namesWithNumber(RoshHashanah1, 2)

    protected override val torah: Torah = parseTorah(Genesis.roshHashanah2torah)

    protected override def maftir: Maftir = RoshHashanah1.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Jeremiah" fromChapter="31" fromVerse="1" toVerse="19"/>).map(_.from(this))
  }

  case object FastOfGedalia extends LoadNames("Fast of Gedalia")
    with NonTishaBeAvFast with PostponeOnShabbos with Parse
  {
    final override def date(year: Year): Day = year.month(Tishrei).day(3)

    protected override val afternoonHaftarahExceptions: Option[Haftarah.Customs] = Some(parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Morocco">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>).map(_.from(this), full = false))
  }

  case object YomKippur extends LoadNames("Yom Kippur")
    with Festival with ShabbosAndWeekdayReadingParse with AfternoonReading
  {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(10)

    protected override val shabbosTorahElement: Elem = Leviticus.yomKippurTorah
    protected override val dropOnWeekday: Set[Int] = Set(2)

    protected override val maftir: Maftir = parseMaftir(Numbers.yomKippurMaftir)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah">
        <custom n="Common" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
        <custom n="Italki, Teiman">
          <part n="1" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
          <part n="2" fromChapter="59" fromVerse="20" toVerse="21"/>
        </custom>
      </haftarah>)

    override def afternoon: Reading = Reading(afternoonTorah, afternoonHaftarah)

    private val afternoonTorah: Torah = parseTorah(Leviticus.yomKippurAfternoonTorah)

    private val afternoonHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah>
        <custom n="Common">
          <part n="1" book="Jonah" fromChapter="1" fromVerse="1" toChapter="4" toVerse="11"/>
          <part n="2" book="Micah" fromChapter="7" fromVerse="18" toVerse="20"/>
        </custom>
        <custom n="Italki">
          <part n="1" book="Obadiah" fromChapter="1" fromVerse="21"/>
          <part n="2" book="Jonah" fromChapter="1" fromVerse="1" toChapter="4" toVerse="11"/>
          <part n="3" book="Micah" fromChapter="7" fromVerse="18" toVerse="20"/>
        </custom>
      </haftarah>)
  }

  case object Succos1 extends LoadNames("Succos") with Festival with FirstDayOf with ShabbosAndWeekdayReadingParse {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(15)

    override val shabbosTorahElement: Elem = Leviticus.succos1and2torah
    override val dropOnWeekday: Set[Int] = Set(2, 4)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Zechariah" toChapter="14" toVerse="21">
        <custom n="Common" fromChapter="14" fromVerse="1"/>
        <custom n="Teiman" fromChapter="13" fromVerse="9"/>
      </haftarah>)

    override def maftir: Maftir = SuccosIntermediate.korbanot.head
  }

  case object Succos2 extends Festival with NonFirstDayOf with ShabbosAndWeekdayReading with Parse {
    override def names: Names = namesWithNumber(Succos1, 2)

    override def firstDay: Date = Succos1

    override def dayNumber: Int = 2

    protected override val shabbosTorah: Torah = Succos1.shabbosTorah
    protected override val weekdayTorah: Torah = Succos1.weekdayTorah

    protected override val maftir: Maftir = Succos1.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings">
        <custom n="Common" fromChapter="8" fromVerse="2" toVerse="21"/>
        <custom n="Italki" fromChapter="7" fromVerse="51" toChapter="8" toVerse="15"/>
        <custom n="Teiman" fromChapter="7" fromVerse="51" toChapter="8" toVerse="21"/>
      </haftarah>)
  }

  private object SuccosIntermediate extends LoadNames("Succos Intermediate") with Parse {
    val korbanot: Seq[Torah.Fragment] = parseTorah(Numbers.succosKorbanot).spans

    val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="38">
        <custom n="Common" fromVerse="18" toChapter="39" toVerse="16"/>
        <custom n="Italki, Teiman" fromVerse="1" toChapter="38" toVerse="23"/>
      </haftarah>)
  }

  sealed class SuccosIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand) with WeekdayReading with ShabbosReading
  {
    final override lazy val names: Names = namesWithNumber(SuccosIntermediate, intermediateDayNumber)

    override def firstDay: Date = Succos1

    override def weekday: Reading = {
      if (intermediateDayNumber == 6) require(inHolyLand)

      // Do not go beyond 6th fragment of korbanot.
      val n: Int = Math.min(intermediateDayNumber, 4)

      val ashkenazAndChabad: Torah = Torah.aliyot(
        korbanot(n),
        korbanot(n+1),
        korbanot(n+2),
        today
      )
      val sefard: Aliyah = today
      val torah: Torah.Customs = new Torah.Customs(Map(
        Custom.Ashkenaz -> ashkenazAndChabad,
        Custom.Chabad -> ashkenazAndChabad,
        Custom.Sefard -> Torah.aliyot(sefard, sefard, sefard, sefard)
      ))

      Reading(torah)
    }

    private def korbanot(n: Int): Aliyah = SuccosIntermediate.korbanot(n)

    private def today: Maftir = {
      val n: Int = intermediateDayNumber
      if (inHolyLand) korbanot(n) else korbanot(n) + korbanot(n+1)
    }

    protected override def shabbosMaftir: Maftir = today

    protected override def shabbosHaftarah: Haftarah.Customs = SuccosIntermediate.shabbosHaftarah
  }

  case object SuccosIntermediate1 extends SuccosIntermediate(1, false)
  case object SuccosIntermediate2 extends SuccosIntermediate(2, false)
  case object SuccosIntermediate3 extends SuccosIntermediate(3, false)
  case object SuccosIntermediate4 extends SuccosIntermediate(4, false)
  case object HoshanahRabbah      extends SuccosIntermediate(5, false)

  case object SuccosIntermediate1InHolyLand extends SuccosIntermediate(1, true)
  case object SuccosIntermediate2InHolyLand extends SuccosIntermediate(2, true)
  case object SuccosIntermediate3InHolyLand extends SuccosIntermediate(3, true)
  case object SuccosIntermediate4InHolyLand extends SuccosIntermediate(4, true)
  case object SuccosIntermediate5InHolyLand extends SuccosIntermediate(5, true)
  case object HoshanahRabbahInHolyLand      extends SuccosIntermediate(6, true)

  case object SheminiAtzeres extends LoadNames("Shemini Atzeres")
    with Festival with NonFirstDayOf with ShabbosAndWeekdayReading with Parse
  {
    override def firstDay: Date = Succos1
    override def dayNumber: Int = 8

    protected override val shabbosTorah: Torah = FestivalEnd.endShabbosTorah
    protected override val weekdayTorah: Torah = FestivalEnd.sheminiAtzeresWeekdayTorah

    override val maftir: Maftir = SuccosIntermediate.korbanot.last

    /*
  Artscroll gives custom Ashkenaz ending at 9:1,
  but @michaelko58 insists that it is the same as Sefard and ends at 8:66.
  His explanation: "there are some ashkenazic communities that follow custom Italki.
  It is possible that this is a difference between chassidim and litaim."
  */
    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings" fromChapter="8" fromVerse="54">
        <custom n="Common" toChapter="8" toVerse="66"/>
        <custom n="Italki, Chabad" toChapter="9" toVerse="1"/>
      </haftarah>)
  }

  sealed trait SimchasTorahCommon extends Festival with NonFirstDayOf with WeekdayReadingSimple with Parse {
    final override def firstDay: Date = Succos1

    protected override val maftir: Maftir = SheminiAtzeres.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Joshua">
        <custom n="Common, Italki" fromChapter="1" fromVerse="1" toVerse="18"/>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="9"/>
          <part n="2" fromChapter="6" fromVerse="27"/>
        </custom>
      </haftarah>)
  }

  case object SimchasTorah extends LoadNames("Simchas Torah") with SimchasTorahCommon with Parse {
    override def dayNumber: Int = 9

    val chassanBereishis: Torah.Fragment = parseTorah(Genesis.chassanBereishis).spans.head

    override val torah: Torah = Parsha.VezosHaberachah.days.common
      .fromWithNumbers(SimchasTorah)
      .to6withLast(chassanBereishis)
  }

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends LoadNames("Shemini Atzeres and Simchas Torah")
    with SimchasTorahCommon with ShabbosReading
  {
    override def dayNumber: Int = 8

    protected override val torah: Torah = SimchasTorah.torah

    override def shabbos: Reading = weekday
  }

  case object ShabbosBereishis extends LoadNames("Shabbos Bereishis") with Date {
    override def date(year: Year): Day = SimchasTorah.date(year).shabbosAfter
  }

  private object Chanukah extends LoadNames("Chanukah") with Parse {
    val day1Cohen: Torah = parseTorah(Numbers.chanukahFirst)

    val korbanot: Seq[Torah.Fragment] = parseTorah(Numbers.chanukahKorbanotSpans).spans
    def first(n: Int): Aliyah = korbanot(2*(n-1))
    def second(n: Int): Aliyah = korbanot(2*(n-1)+1)
    val zos: Torah.Fragment = korbanot.last

    val haftarahShabbos1: Haftarah.Customs = parseHaftarah(
        <haftarah book="Zechariah" fromChapter="2" fromVerse="14" toChapter="4" toVerse="7"/>)

    val haftarahShabbos2: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings" fromChapter="7" fromVerse="40" toChapter="7">
        <custom n="Common" toVerse="50"/>
        <custom n="Italki" toVerse="51"/>
      </haftarah>)
  }

  sealed class Chanukah(override val dayNumber: Int) extends WithNames with DayOf with RabbinicFestival {
    private def first(n: Int): Aliyah = Chanukah.first(n)
    private def second(n: Int): Aliyah = Chanukah.second(n)
    private def split(n: Int): Seq[Aliyah] = Seq(first(n), second(n))
    private def full(n: Int): Aliyah = first(n)+second(n)

    final override lazy val names: Names = namesWithNumber(Chanukah, dayNumber)

    final override def firstDay: Date = Chanukah1

    final override def date(year: Year): Day = year.month(Kislev).day(25)+(dayNumber-1)

    final def shabbos(weeklyReading: WeeklyReading, isRoshChodesh: Boolean): Reading = {
      val result = replaceMaftirAndHaftarah(weeklyReading.getMorningReading,
        maftir = full(dayNumber).from(this),
        haftarah = if (dayNumber < 8) Chanukah.haftarahShabbos1 else Chanukah.haftarahShabbos2)

      if (!isRoshChodesh) result else RoshChodesh.addShabbosMaftirAs7thAliyah(result)
    }

    final def weekday(isRoshChodesh: Boolean): Reading = {
      val (
        ashkenazAndChabad: Seq[Aliyah],
        sefard: Seq[Aliyah]
      ) = if (dayNumber == 1) {
        val day1CohenAshkenazAndChabad: Torah.Fragment = Chanukah.day1Cohen.spans(1)
        val day1CohenSefard: Torah.Fragment = Chanukah.day1Cohen.spans.head + day1CohenAshkenazAndChabad
        (
          day1CohenAshkenazAndChabad +: split(dayNumber),
          day1CohenSefard +: split(dayNumber)
        )
      } else if (dayNumber != 8) (
        split(dayNumber) :+ full(dayNumber+1),
        split(dayNumber) :+ full(dayNumber)
      ) else (
        split(dayNumber) :+ Chanukah.zos,
        split(dayNumber) :+ (full(dayNumber) + Chanukah.zos)
      )

      require(ashkenazAndChabad.length == 3)
      require(sefard.length == 3)

      var torah: Torah.Customs = new Torah.Customs(Map[Custom, Torah](
        Custom.Ashkenaz -> Torah(ashkenazAndChabad),
        Custom.Chabad -> Torah(ashkenazAndChabad),
        Custom.Sefard -> Torah(sefard)
      )).map(_.from(this))

      val torahResult: Torah.Customs =
        if (!isRoshChodesh) torah
        else Custom.Of(RoshChodesh.in3aliyot :+ full(dayNumber).from(this))

      Reading(torahResult)
    }
  }

  case object Chanukah1 extends Chanukah(1)
  case object Chanukah2 extends Chanukah(2)
  case object Chanukah3 extends Chanukah(3)
  case object Chanukah4 extends Chanukah(4)
  case object Chanukah5 extends Chanukah(5)
  case object Chanukah6 extends Chanukah(6)
  case object Chanukah7 extends Chanukah(7)
  case object Chanukah8 extends Chanukah(8)

  case object FastOfTeves extends LoadNames("Fast of 10th of Teves") with NonTishaBeAvFast {
    override def date(year: Year): Day = year.month(Teves).day(10)
  }

  case object FastOfEster extends LoadNames("Fast of Ester") with NonTishaBeAvFast {
    override def date(year: Year): Day = Purim.date(year)-1

    protected override def correctDate(result: Day): Day =
      // If on Friday or Saturday - move to Thursday
      if (result.isShabbos) result-2 else
      if (result.next.isShabbos) result-1 else
        result
  }

  sealed trait SpecialShabbos extends Date with Parse

  sealed trait SpecialParsha extends SpecialShabbos with MaftrirAndHaftarah {
    final def transform(reading: Reading, isRoshChodesh: Boolean): Reading = {
      val result = replaceMaftirAndHaftarah(reading, maftir, haftarah)
      if (!isRoshChodesh) result else RoshChodesh.addShabbosMaftirAs7thAliyah(result)
    }
  }

  case object ParshasShekalim extends LoadNames("Parshas Shekalim") with SpecialParsha {
    override def date(year: Year): Day = {
      val result = Purim.date(year).month.firstDay
      if (result.isShabbos) result else result.shabbosBefore
    }

    protected override def maftir: Maftir = parseMaftir(Exodus.parshasShekalimMaftir)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="II Kings">
        <custom n="Ashkenaz, Italki, Teiman, Chabad" fromChapter="12" fromVerse="1" toVerse="17"/>
        <custom n="Sefard" fromChapter="11" fromVerse="17" toChapter="12" toVerse="17"/>
      </haftarah>)
  }

  case object ParshasZachor extends LoadNames("Parshas Zachor") with SpecialParsha {
    override def date(year: Year): Day = Purim.date(year).shabbosBefore

    protected override val maftir: Maftir = parseMaftir(Deuteronomy.parshasZachorMaftir)

    override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel">
        <custom n="Ashkenaz, Chabad" fromChapter="15" fromVerse="2" toVerse="34"/>
        <custom n="Sefard" fromChapter="15" fromVerse="1" toVerse="34"/>
        <custom n="Teiman" fromChapter="14" fromVerse="52" toChapter="15" toVerse="33"/>
      </haftarah>)
  }

  case object ParshasParah extends LoadNames("Parshas Parah") with SpecialParsha {
    override def date(year: Year): Day = ParshasHachodesh.date(year).shabbosBefore

    protected override val maftir: Maftir = parseMaftir(Numbers.parshasParahMaftir)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="36" fromVerse="16">
        <custom n="Ashkenaz, Italki" toVerse="38"/>
        <custom n="Sefard" toVerse="36"/>
      </haftarah>)
  }

  case object ParshasHachodesh extends LoadNames("Parshas Hachodesh") with SpecialParsha {
    override def date(year: Year): Day = {
      val result = year.month(Nisan).firstDay
      if (result.isShabbos) result else result.shabbosBefore
    }

    protected override def maftir: Maftir = parseMaftir(Exodus.parshasHachodeshMaftir)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="45" toChapter="46">
        <custom n="Ashkenaz" fromVerse="16" toVerse="18"/>
        <custom n="Italki" fromVerse="18" toVerse="11"/>
        <custom n="Sefard" fromVerse="18" toVerse="15"/>
        <custom n="Teiman" fromVerse="9" toVerse="11"/>
      </haftarah>)
  }

  case object ShabbosHagodol extends LoadNames("Shabbos Hagodol") with SpecialShabbos {
    override def date(year: Year): Day = Pesach.date(year).shabbosBefore

    def transform(isErevPesach: Boolean, reading: Reading): Reading =
      reading.transform[Haftarah](haftarah, {
        case (custom: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
          if ((custom == Custom.Chabad) && !isErevPesach) readingCustom
          else readingCustom.replaceHaftarah(haftarah)
      })

    private val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="Malachi" fromChapter="3" fromVerse="4" toVerse="24"/>)
  }

  sealed trait PurimCommon extends RabbinicFestival with WeekdayReading

  case object Purim extends LoadNames("Purim") with PurimCommon with Parse {
    override def date(year: Year): Day = year.latestAdar.day(14)

    val torah: Torah = parseTorah(Exodus.purimTorah)

    override def weekday: Reading = Reading(torah)
  }

  case object ShushanPurim extends LoadNames("Shushan Purim") with PurimCommon {
    override def date(year: Year): Day = Purim.date(year) + 1

    override def weekday: Reading = Reading(Purim.torah)

    final def shabbos(weeklyReading: WeeklyReading): Reading = {
      replaceMaftirAndHaftarah(weeklyReading.getMorningReading,
        maftir = shushanPurimShabbosMaftir,
        haftarah = ParshasZachor.haftarah)
    }

    private val shushanPurimShabbosMaftir: Torah.Aliyah = {
      // TODO use merge instead:
      val spans = Purim.torah.spans
      spans(0)+spans(1)+spans(2)
    }
  }

  case object Pesach extends LoadNames("Pesach") with Festival with FirstDayOf with ShabbosAndWeekdayReadingParse {
    final def date(year: Year): Day = year.month(Nisan).day(15)

    protected override val shabbosTorahElement: Elem = Exodus.pesach1torah
    protected override val dropOnWeekday: Set[Int] = Set(4, 7)

    override val maftir: Maftir = parseMaftir(Numbers.pesachMaftir)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Joshua">
        <custom n="Ashkenaz, Chabad">
          <part n="1" fromChapter="3" fromVerse="5" toVerse="7"/>
          <part n="2" fromChapter="5" fromVerse="2" toChapter="6" toVerse="1"/>
          <part n="3" fromChapter="6" fromVerse="27"/>
        </custom>
        <custom n="Sefard">
          <part n="1" fromChapter="5" fromVerse="2" toChapter="6" toVerse="1"/>
          <part n="2" fromChapter="6" fromVerse="27"/>
        </custom>
        <custom n="Frankfurt, Hagra" fromChapter="5" fromVerse="2" toChapter="6" toVerse="1"/>
      </haftarah>)
  }

  case object Pesach2 extends Festival with NonFirstDayOf with WeekdayReadingSimple with Parse {
    override lazy val names: Names = namesWithNumber(Pesach, 2)

    override def firstDay: Date = Pesach

    override def dayNumber: Int = 2

    override def torah: Torah = Succos1.weekdayTorah // TODO .fromWithNumbers(this)?

    protected override val maftir: Maftir = Pesach.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="II Kings">
        <custom n="Common">
          <part n="1" fromChapter="23" fromVerse="1" toVerse="9"/>
          <part n="2" fromChapter="23" fromVerse="21" toVerse="25"/>
        </custom>
        <custom n="Italki" fromChapter="23" fromVerse="21" toVerse="30"/>
        <custom n="Teiman">
          <part n="1" fromChapter="22" fromVerse="1" toVerse="7"/>
          <part n="2" fromChapter="23" fromVerse="21" toVerse="25"/>
        </custom>
      </haftarah>)
  }

  private object PesachIntermediate extends LoadNames("Pesach Intermediate") with Parse {
    def first5(realDayNumber: Int): Torah = realDayNumber match {
      case 2 => day2
      case 3 => day3
      case 4 => day4
      case 5 => day5
      case 6 => day6
    }

    // TODO add numbers to names (AndNumber)?
    private val day2: Torah = Pesach2.torah.drop(Set(4, 5))
    private val day3: Torah = parseTorah(Exodus.pesach3torah)
    private val day4: Torah = parseTorah(Exodus.pesach4torah)
    private val day5: Torah = {
      val all = IntermediateShabbos.torah.spans
      Torah.aliyot( // Exodus.pesach5torah(this)
        all(3),         // Exodus 34:1-3
        all(4)+all(5),  // Exodus 34:4-17
        all(6)          // Exodus 34:18-26
      )
    }
    private val day6: Torah = parseTorah(Numbers.pesach6torah)

    val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel">
        <custom n="Common" fromChapter="37" fromVerse="1" toVerse="14"/>
        <custom n="Teiman" fromChapter="36" fromVerse="37" toChapter="37" toVerse="14"/>
      </haftarah>)
  }

  sealed class PesachIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand)
  {
    final override lazy val names: Names = namesWithNumber(PesachIntermediate, intermediateDayNumber)

    final override def firstDay: Date = Pesach

    protected final override def shabbosMaftir: Maftir = Pesach7.maftir

    protected final override def shabbosHaftarah: Haftarah.Customs = PesachIntermediate.shabbosHaftarah

    final def weekday(isPesachOnChamishi: Boolean): Reading = {
      val realDayNumber: Int =
        if (isPesachOnChamishi && ((dayNumber == 4) || (dayNumber == 5))) dayNumber-1 else dayNumber
      Reading(PesachIntermediate.first5(realDayNumber) :+ shabbosMaftir)
    }
  }

  case object PesachIntermediate1 extends PesachIntermediate(1, false)
  case object PesachIntermediate2 extends PesachIntermediate(2, false)
  case object PesachIntermediate3 extends PesachIntermediate(3, false)
  case object PesachIntermediate4 extends PesachIntermediate(4, false)

  case object PesachIntermediate1InHolyLand extends PesachIntermediate(1, true)
  case object PesachIntermediate2InHolyLand extends PesachIntermediate(2, true)
  case object PesachIntermediate3InHolyLand extends PesachIntermediate(3, true)
  case object PesachIntermediate4InHolyLand extends PesachIntermediate(4, true)
  case object PesachIntermediate5InHolyLand extends PesachIntermediate(5, true)

  case object Pesach7 extends LoadNames("Pesach 7") with Festival with NonFirstDayOf with ShabbosAndWeekdayReadingParse {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 7

    protected override val shabbosTorahElement: Elem = Exodus.pesach7torah
    protected override val dropOnWeekday: Set[Int] = Set(2, 4)

    override val maftir: Maftir = parseMaftir(Numbers.pesachEndMaftir)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="II Samuel" fromChapter="22" fromVerse="1" toVerse="51"/>)
  }

  case object Pesach8 extends LoadNames("Pesach 8")
    with Festival with NonFirstDayOf with ShabbosAndWeekdayReading with Parse
  {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 8

    protected override val shabbosTorah: Torah = FestivalEnd.endShabbosTorah
    protected override val weekdayTorah: Torah = FestivalEnd.endWeekdayTorah

    protected override val maftir: Maftir = Pesach7.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="Isaiah" fromChapter="10" fromVerse="32" toChapter="12" toVerse="6"/>)
  }

  case class Omer(number: Int) extends WithNames {
    override def names: Names = namesWithNumber(Omer, number)
  }

  object Omer extends LoadNames("Omer") {
    def dayOf(day: Day): Option[Omer] = {
      val year = day.year
      val pesach = Pesach.date(year)
      val shavous = Shavuos.date(year)
      if ((day <= pesach) || (day >= shavous)) None else Some(Omer(day - pesach))
    }
  }

  case object LagBaOmer extends LoadNames("Lag Ba Omer") with Date {
    override def date(year: Year): Day = Pesach.date(year) + 33
  }

  case object Shavuos extends LoadNames("Shavuos")
    with Festival with FirstDayOf with WeekdayReadingSimple with Parse
  {
    override def date(year: Year): Day = Pesach.date(year) + 50

    protected override def torah: Torah = parseTorah(Exodus.shavuosTorah)

    override def maftir: Maftir = parseMaftir(Numbers.shavuosMaftir)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel">
        <custom n="Common">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="28"/>
          <part n="2" fromChapter="3" fromVerse="12"/>
        </custom>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toChapter="2" toVerse="2"/>
          <part n="2" fromChapter="3" fromVerse="12"/>
        </custom>
      </haftarah>)
  }

  case object Shavuos2 extends Festival with NonFirstDayOf with ShabbosAndWeekdayReading with Parse {
    override lazy val names: Names = namesWithNumber(Shavuos, 2)

    override def firstDay: Date = Shavuos

    override def dayNumber: Int = 2

    protected override val shabbosTorah: Torah = FestivalEnd.endShabbosTorah
    protected override val weekdayTorah: Torah = FestivalEnd.endWeekdayTorah

    protected override val maftir: Maftir = Shavuos.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Habakkuk">
        <custom n="Ashkenaz" fromChapter="3" fromVerse="1" toVerse="19"/>
        <custom n="Sefard" fromChapter="2" fromVerse="20" toChapter="3" toVerse="19"/>
      </haftarah>)
  }

  case object FastOfTammuz extends LoadNames("Fast of Tammuz") with NonTishaBeAvFast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tammuz).day(17)
  }

  case object TishaBeAv extends LoadNames("Tisha BeAv") with Fast with PostponeOnShabbos with Parse {
    override def date(year: Year): Day = year.month(Av).day(9)

    override def weekday: Reading = Reading(torah, haftarah)

    private val torah: Torah = parseTorah(Deuteronomy.tishaBeAvTorah)

    private val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Jeremiah">
        <custom n="Common" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
        <custom n="Teiman">
          <part n="1" fromChapter="6" fromVerse="16" toVerse="17"/>
          <part n="2" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
        </custom>
      </haftarah>)
  }

  private def namesWithNumber(withNames: WithNames, number: Int): Names =
    withNames.names.transform(name => name.copy(name.name + " " + name.languageSpec.toString(number)))

  val festivals: Set[FestivalOrIntermediate] = Set(
    RoshHashanah1, RoshHashanah2,
    YomKippur,
    Succos1, Succos2,
    SuccosIntermediate1, SuccosIntermediate2, SuccosIntermediate3, SuccosIntermediate4,
    HoshanahRabbah, SheminiAtzeres, SimchasTorah,
    Pesach, Pesach2, PesachIntermediate1, PesachIntermediate2, PesachIntermediate3, PesachIntermediate4, Pesach7, Pesach8,
    Shavuos, Shavuos2
  )

  val festivalsInHolyLand: Set[FestivalOrIntermediate] = Set(
    RoshHashanah1, RoshHashanah2,
    YomKippur,
    Succos1,
    SuccosIntermediate1InHolyLand, SuccosIntermediate2InHolyLand, SuccosIntermediate3InHolyLand,
    SuccosIntermediate4InHolyLand, SuccosIntermediate5InHolyLand,
    HoshanahRabbahInHolyLand, SheminiAtzeresAndSimchasTorahInHolyLand,
    Pesach, PesachIntermediate1InHolyLand, PesachIntermediate2InHolyLand, PesachIntermediate3InHolyLand,
    PesachIntermediate4InHolyLand, PesachIntermediate5InHolyLand, Pesach7,
    Shavuos
  )

  def festivals(inHolyLand: Boolean): Set[FestivalOrIntermediate] = if (inHolyLand) festivalsInHolyLand else festivals

  val rabbinicFestivals: Set[RabbinicFestival] = Set(
    Chanukah1, Chanukah2, Chanukah3, Chanukah4, Chanukah5, Chanukah6, Chanukah7, Chanukah8,
    Purim, ShushanPurim
  )

  val fasts: Set[Fast] = Set(FastOfGedalia, FastOfTeves, FastOfEster, FastOfTammuz, TishaBeAv)

  val daysWithSpecialReadingsNotFestivals: Set[Date] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[Date] = festivals(inHolyLand) ++ daysWithSpecialReadingsNotFestivals

  val specialShabbos: Set[SpecialShabbos] =
    Set(ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh, ShabbosHagodol)

  final def getMorningReading(
    day: Day,
    specialDay: Option[Date],
    specialShabbos: Option[SpecialShabbos],
    weeklyReading: Option[WeeklyReading],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] = {
    val isShabbos: Boolean = day.isShabbos
    if (!isShabbos) require(weeklyReading.isEmpty && specialShabbos.isEmpty)

    val result =
      if (isShabbos) Some(getShabbosMorningReading(day, specialDay, weeklyReading, specialShabbos))
      else getWeekdayMorningReading(day, specialDay, nextWeeklyReading, isPesachOnChamishi)

    val numAliyot: Int =
      if (specialDay.contains(SimchasTorah)) 7 else
      if (specialDay.contains(SheminiAtzeresAndSimchasTorahInHolyLand)) 7 else
      if (specialDay.contains(YomKippur)) 6 else
      if (isShabbos) 7 else
      if (specialDay.exists(_.isInstanceOf[Festival])) 5 else
      if (specialDay.exists(_.isInstanceOf[Intermediate])) 4 else
      if (day.isRoshChodesh) 4 else
      if (specialDay.exists(_.isInstanceOf[RabbinicFestival])) 3 else
      if (specialDay.exists(_.isInstanceOf[Fast])) 3 else
      if (day.is(Day.Name.Sheni) || day.is(Day.Name.Chamishi)) 3 else
        0

    result.fold(require(numAliyot == 0)) { result =>
      result.torah.customs.values.foreach { torah =>
        require((torah.length == numAliyot) || ((numAliyot == 3) && (torah.length == 2))) // because maftir!
      }
    }

    result
  }

  final def getPurimAlternativeMorningReading(
    day: Day,
    specialDay: Option[Date],
    specialShabbos: Option[SpecialShabbos],
    weeklyReading: Option[WeeklyReading],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] = {
    val isAlternative = specialDay.contains(Purim) || specialDay.contains(ShushanPurim)
    if (!isAlternative) None else getMorningReading(day, None, specialShabbos, weeklyReading, nextWeeklyReading, isPesachOnChamishi)
  }

  private final def getShabbosMorningReading(
    day: Day,
    specialDay: Option[Date],
    weeklyReading: Option[WeeklyReading],
    specialShabbos: Option[SpecialShabbos],
  ): Reading = {
    require(day.isShabbos)

    val isRoshChodesh: Boolean = day.isRoshChodesh

    val normal: Reading = specialDay.map {
      case specialDay: ShabbosReading =>
        require(weeklyReading.isEmpty)
        specialDay.shabbos

      case specialDay: Chanukah =>
        require(weeklyReading.isDefined)
        specialDay.shabbos(weeklyReading.get, isRoshChodesh)

      case ShushanPurim =>
        require(weeklyReading.isDefined)
        ShushanPurim.shabbos(weeklyReading.get)

      case _ =>
        throw new IllegalArgumentException("Must have Shabbos reading!")
    }
      .getOrElse {
        require(weeklyReading.isDefined)
        val result = weeklyReading.get.getMorningReading
        val isKiSeitzei: Boolean = (day.month.name == Elul) && (day.numberInMonth == 14)
        if (!isKiSeitzei) result else {
          val customs: Custom.Of[Reading.ReadingCustom] = result.liftR {
            case (custom: Custom, readingCustom: Reading.ReadingCustom) =>
              if (custom != Custom.Chabad) readingCustom
              else readingCustom.addHaftarah(Haftarah.forParsha(Parsha.Re_eh).doFind(Custom.Chabad))
          }
          new Reading(customs.customs)
        }
      }

    val result = specialShabbos.fold(normal) {
      case s: SpecialParsha => s.transform(normal, isRoshChodesh)
      case ShabbosHagodol =>
        ShabbosHagodol.transform(isErevPesach = day.next == Pesach.date(day.year), normal)
    }

    val roshChodeshOf: Option[Month.Name] = {
      val result = day.roshChodeshOf
      // We do not mention Rosh Chodesh on Rosh Hashanah
      if (result.contains(Tishrei)) None else result
    }

    val isSpecialShabbos: Boolean = specialShabbos.isDefined

    val correctedForRoshChodesh = roshChodeshOf
      .fold(result)(month =>
        RoshChodesh.correct(month, isSpecialShabbos, result))

    day.next.roshChodeshOf
      .fold(correctedForRoshChodesh)(month =>
        ErevRoshChodesh.correct(month, roshChodeshOf.isDefined, isSpecialShabbos, correctedForRoshChodesh))
  }

  private final val sheniAndChamishi: Set[Day.Name] = Set(Day.Name.Sheni, Day.Name.Chamishi)

  private final def getWeekdayMorningReading(
    day: Day,
    specialDay: Option[Date],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] = {
    val isRoshChodesh: Boolean = day.isRoshChodesh
    val specialReading: Option[Reading] = specialDay.map {
      case specialDay: WeekdayReading =>
        specialDay.weekday

      case specialDay: Chanukah =>
        specialDay.weekday(isRoshChodesh)

      case specialDay: PesachIntermediate =>
        specialDay.weekday(isPesachOnChamishi)

      case _ =>
        throw new IllegalArgumentException("Must have weekday reading!")
    }

    specialReading
      .orElse { if (!isRoshChodesh) None else Some(RoshChodesh.weekday) }
      .orElse { if (!sheniAndChamishi.contains(day.name)) None else Some(nextWeeklyReading.getAfternoonReading) }
  }

  // On Festival that falls on Shabbos, afternoon reading is that of the Shabbos - except on Yom Kippur.
  def getAfternoonReading(
    day: Day,
    specialDay: Option[Date],
    nextWeeklyReading: WeeklyReading
  ): Option[Reading] = {
    val specialReading: Option[Reading] = specialDay flatMap {
      case specialDay: AfternoonReading =>
        Some(specialDay.afternoon)

      case _ => None
    }

    val result = specialReading.orElse { if (!day.isShabbos) None else Some(nextWeeklyReading.getAfternoonReading) }

    result.foreach { result =>
      result.torah.customs.values.foreach { torah =>
        require((torah.length == 3) || (torah.length == 2)) // 3rd aliyah is also maftir...
      }
    }

    result
  }

  private val loadNames: Seq[LoadNames] = Seq(
    FestivalEnd, IntermediateShabbos, RoshChodesh, ErevRoshChodesh, Fast,
    RoshHashanah1, FastOfGedalia, YomKippur, Succos1,
    SuccosIntermediate, SheminiAtzeres, SimchasTorah, SheminiAtzeresAndSimchasTorahInHolyLand,
    ShabbosBereishis, Chanukah, FastOfTeves,
    ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh, ShabbosHagodol,
    FastOfEster, Purim, ShushanPurim, Pesach, PesachIntermediate, Pesach7, Pesach8,
    Omer, LagBaOmer, Shavuos, FastOfTammuz, TishaBeAv
  )

  private val toNames: Map[WithName, Names] = Metadata.loadNames(loadNames, this, "SpecialDay")
}
