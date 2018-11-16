package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata._
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.Custom
import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.calendar.jewish.Jewish.Month.Name._
import org.podval.calendar.jewish.{JewishDay, JewishYear}

import scala.xml.Elem

object SpecialDay {

  sealed class LoadNames(override val name: String) extends WithName with WithNames {
    final override def names: Names = toNames(this)
  }

  type Torah = Seq[ChumashSpan.BookSpan]

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
    protected def maftir: ChumashSpan.BookSpan

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

  sealed trait AfternoonReading {
    def afternoon: Reading
  }

  sealed trait FestivalOrIntermediate extends Date

  sealed trait Festival extends FestivalOrIntermediate with WeekdayReading

  sealed abstract class Intermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends FestivalOrIntermediate with NonFirstDayOf with ShabbosReading
  {
    final override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)

    final override def shabbos: Reading = Reading(
      torah = Exodus.intermediateShabbosTorah,
      maftir = shabbosMaftir,
      haftarah = shabbosHaftarah
    )

    protected def shabbosMaftir: ChumashSpan.BookSpan

    protected def shabbosHaftarah: Haftarah.Customs
  }

  sealed trait RabbinicFestival extends Date

  private def replaceMaftirAndHaftarah(
    reading: Reading,
    maftir: ChumashSpan.BookSpan,
    haftarah: Haftarah.Customs
  ): Reading = reading.transformR[Haftarah.Haftarah](haftarah, transformer = {
    case (custom: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah.Haftarah) =>
      readingCustom.replaceMaftirAndHaftarah(maftir, haftarah)
  })

  sealed trait MaftirAndHaftarahTransform {
    protected final def transform(
      transformer: (
        Custom,
        Reading.ReadingCustom,
        Haftarah.Haftarah,
        Option[Haftarah.Haftarah]
      ) => Reading.ReadingCustom,
      reading: Reading
    ): Reading = reading.transformR[(Haftarah.Haftarah, Option[Haftarah.Haftarah])](haftarahs, { case (
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarahs: (Haftarah.Haftarah, Option[Haftarah.Haftarah])
      ) =>
        val (haftarah: Haftarah.Haftarah, addition: Option[Haftarah.Haftarah]) = haftarahs
        transformer(custom, reading, haftarah, addition)
    })

    private lazy val haftarahs: Custom.Of[(Haftarah.Haftarah, Option[Haftarah.Haftarah])] =
      shabbosHaftarah * shabbosAdditionalHaftarah

    protected def shabbosHaftarah: Haftarah.Customs

    protected def shabbosAdditionalHaftarah: Haftarah.Customs
  }

  case object RoshChodesh extends LoadNames("Rosh Chodesh") with WeekdayReading with MaftirAndHaftarahTransform {
    private val torah: Torah = Numbers.roshChodesh

    <torah book="Numbers" fromChapter="28" fromVerse="1" toVerse="15">
      <aliyah n="2" fromVerse="3"/>
      <aliyah n="3" fromVerse="4"/>
      <aliyah n="4" fromVerse="6"/>
      <aliyah n="5" fromVerse="9"/>
      <aliyah n="6" fromVerse="11"/>
    </torah>
    override def weekday: Reading = Reading(
      Seq(
        torah.head+torah(1), // 1-3
        torah(1)+torah(2),   // 3-5
        torah(3)+torah(4),   // 6-10
        torah(5)             // 11-15
      )
    )

    def in3aliyot: Torah = Seq(
      torah.head+torah(1)+torah(2), // 1-5
      torah(3)+torah(4),            // 6-10
      torah(5)                      // 11-15
    )

    def shabbosChanukahTorah: ChumashSpan.BookSpan = torah(4)+torah(5) // 9-15

    def correct(month: Month.Name, isSpecialShabbos: Boolean, reading: Reading): Reading = {
      val allowReplace: Boolean = !isSpecialShabbos && (month != Teves) && (month != Av)
      def transformer(
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarah: Haftarah.Haftarah,
        addition: Option[Haftarah.Haftarah]
      ): Reading.ReadingCustom = {
        if (allowReplace && ((month != Elul) || (custom == Custom.Chabad)))
          reading.replaceMaftirAndHaftarah(shabbosMaftir, haftarah)
        else
          addition.fold(reading)(addition => reading.addHaftarah(addition))
      }

      transform(transformer, reading)
    }

    private val shabbosMaftir: ChumashSpan.BookSpan = torah(4) + torah(5)

    protected override val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="66">
        <part n="1" fromVerse="1" toVerse="24"/>
        <part n="2" fromVerse="23" toVerse="23"/>
      </haftarah>)

    protected override val shabbosAdditionalHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Chabad" book="Isaiah" fromChapter="66">
          <part n="1" fromVerse="1" toVerse="1"/>
          <part n="2" fromVerse="23" toVerse="24"/>
          <part n="3" fromVerse="23" toVerse="23"/>
        </custom>
      </haftarah>)
  }

  case object ErevRoshChodesh extends LoadNames("Erev Rosh Chodesh") with MaftirAndHaftarahTransform {
    def correct(month: Month.Name, isSpecialShabbos: Boolean, isRoshChodesh: Boolean, reading: Reading): Reading = {
      val allowReplace: Boolean = specialShabbos.isEmpty && !isRoshChodesh &&
        (month != Teves) && (month != Av) && (month != Elul)

      def transformer(
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarah: Haftarah.Haftarah,
        addition: Option[Haftarah.Haftarah]
      ): Reading.ReadingCustom = {
        if (allowReplace && (custom != Custom.Fes))
          reading.replaceHaftarah(haftarah)
        else
          addition.fold(reading)(addition => reading.addHaftarah(addition))
      }

      transform(transformer, reading)
    }

    protected override val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="I Samuel" fromChapter="20" fromVerse="18" toVerse="42"/>)

    protected override val shabbosAdditionalHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Chabad, Fes" book="I Samuel" fromChapter="20">
          <part n="1" fromVerse="18" toVerse="18"/>
          <part n="2" fromVerse="42" toVerse="42"/>
        </custom>
      </haftarah>)
  }

  private object Fast {
    val defaultAfternoonHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Ashkenaz, Chabad, Morocco" book="Isaiah" fromChapter="55" fromVerse="6" toChapter="56" toVerse="8"/>
        <custom n="Algeria">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>)
  }

  sealed trait Fast extends Date with WeekdayReading with AfternoonReading {
    final override def afternoon: Reading = {
      val torah = Exodus.fastTorah
      val haftarah = afternoonHaftarah

      new Reading(haftarah.lift { case (custom: Custom, haftarah: Option[Haftarah.Haftarah]) =>
        haftarah.fold(Reading.ReadingCustom(torah, None)) { haftarah: Haftarah.Haftarah =>
          Reading.ReadingCustom(torah.init, Some(Reading.MaftirAndHaftarah(torah.last, haftarah)))
        }
      })
    }

    private def afternoonHaftarah: Haftarah.Customs =
      afternoonHaftarahExceptions.fold(Fast.defaultAfternoonHaftarah) { afternoonHaftarahExceptions =>
        Fast.defaultAfternoonHaftarah ++ afternoonHaftarahExceptions }

    protected val afternoonHaftarahExceptions: Option[Haftarah.Customs] = None
  }

  sealed trait NonTishaBeAvFast extends Fast {
    final override def weekday: Reading = Reading(Exodus.fastTorah)
  }

  case object RoshHashanah1 extends LoadNames("Rosh Hashanah") with Festival with ShabbosAndWeekdayReading {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(1)

    protected override val (shabbosTorah: Torah, weekdayTorah: Torah) = Genesis.roshHashana1torah

    protected override val maftir: ChumashSpan.BookSpan = Numbers.roshHashanahMaftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="I Samuel" fromChapter="1" fromVerse="1" toChapter="2" toVerse="10"/>)
  }

  case object RoshHashanah2 extends Festival with NonFirstDayOf with WeekdayReadingSimple {
    override def firstDay: Date = RoshHashanah1

    override def dayNumber: Int = 2

    override lazy val names: Names = namesWithNumber(RoshHashanah1, 2)

    protected override val torah: Torah = Genesis.roshHashanah2torah

    protected override def maftir: ChumashSpan.BookSpan = Numbers.roshHashanahMaftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Jeremiah" fromChapter="31" fromVerse="1" toVerse="19"/>)
  }

  case object FastOfGedalia extends LoadNames("Fast of Gedalia") with NonTishaBeAvFast with PostponeOnShabbos {
    final override def date(year: Year): Day = year.month(Tishrei).day(3)

    protected override val afternoonHaftarahExceptions: Option[Haftarah.Customs] = Some(parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Morocco">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>))
  }

  case object YomKippur extends LoadNames("Yom Kippur") with Festival with ShabbosAndWeekdayReading with AfternoonReading {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(10)

    protected override val (shabbosTorah, weekdayTorah) = Leviticus.yomKippurTorah

    protected override val maftir: ChumashSpan.BookSpan = Numbers.yomKippurMaftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah">
        <custom n="Common" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
        <custom n="Italki, Teiman">
          <part n="1" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
          <part n="2" fromChapter="59" fromVerse="20" toVerse="21"/>
        </custom>
      </haftarah>)

    override def afternoon: Reading = Reading(afternoonTorah, afternoonHaftarah)

    private val afternoonTorah: Torah = Leviticus.yomKippurAfternoonTorah

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

  case object Succos1 extends LoadNames("Succos") with Festival with FirstDayOf with ShabbosAndWeekdayReading {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(15)

    override val (shabbosTorah: Torah, weekdayTorah: Torah) = Leviticus.succos1and2torah

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Zechariah" toChapter="14" toVerse="21">
        <custom n="Common" fromChapter="14" fromVerse="1"/>
        <custom n="Teiman" fromChapter="13" fromVerse="9"/>
      </haftarah>)

    override def maftir: ChumashSpan.BookSpan = Numbers.succosKorbanot.head
  }

  case object Succos2 extends Festival with NonFirstDayOf with ShabbosAndWeekdayReading {
    override def names: Names = namesWithNumber(Succos1, 2)

    override def firstDay: Date = Succos1

    override def dayNumber: Int = 2

    protected override val (shabbosTorah: Torah, weekdayTorah: Torah)  = Leviticus.succos1and2torah

    protected override val maftir: ChumashSpan.BookSpan = Succos1.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings">
        <custom n="Common" fromChapter="8" fromVerse="2" toVerse="21"/>
        <custom n="Italki" fromChapter="7" fromVerse="51" toChapter="8" toVerse="15"/>
        <custom n="Teiman" fromChapter="7" fromVerse="51" toChapter="8" toVerse="21"/>
      </haftarah>)
  }

  private object SuccosIntermediate extends LoadNames("Succos Intermediate") {
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

      val n: Int = Math.min(dayNumber, 5)

      val torah: Custom.Of[Torah] = new Custom.Of[Torah](Map(
        Custom.Common -> (Seq(korbanot(n), korbanot(n+1), korbanot(n+2)) :+ shabbosMaftir),
        Custom.RavNaeHolyLand -> Seq(korbanot(n), korbanot(n), korbanot(n), korbanot(n))
      ))

      Reading(torah)
    }

    private def korbanot(n: Int): ChumashSpan.BookSpan = Numbers.succosKorbanot(n-1)

    protected override def shabbosMaftir: ChumashSpan.BookSpan =
      if (inHolyLand) korbanot(dayNumber) else korbanot(dayNumber-1) + korbanot(dayNumber)

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

  case object SheminiAtzeres extends LoadNames("Shemini Atzeres") with Festival with NonFirstDayOf
    with ShabbosAndWeekdayReading
  {
    override def firstDay: Date = Succos1
    override def dayNumber: Int = 8

    protected override val (shabbosTorah: Torah, weekdayTorah: Torah) = Deuteronomy.sheminiAtzeresTorah

    protected override val maftir: ChumashSpan.BookSpan = Numbers.sheminiAtzeresMaftir

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

  sealed trait SimchasTorahCommon extends Festival with NonFirstDayOf with WeekdayReadingSimple {
    final override def firstDay: Date = Succos1

    protected override val torah: Torah = Deuteronomy.zosHaberachaIn6 ++ Genesis.chassanBereishis

    protected override val maftir: ChumashSpan.BookSpan = Numbers.sheminiAtzeresMaftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Joshua">
        <custom n="Common, Italki" fromChapter="1" fromVerse="1" toVerse="18"/>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="9"/>
          <part n="2" fromChapter="6" fromVerse="27"/>
        </custom>
      </haftarah>)
  }

  case object SimchasTorah extends LoadNames("Simchas Torah") with SimchasTorahCommon {
    override def dayNumber: Int = 9
  }

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends LoadNames("Shemini Atzeres and Simchas Torah")
    with SimchasTorahCommon with ShabbosReading
  {
    override def dayNumber: Int = 8

    override def shabbos: Reading = weekday
  }

  case object ShabbosBereishis extends LoadNames("Shabbos Bereishis") with Date {
    override def date(year: Year): Day = SimchasTorah.date(year).shabbosAfter
  }

  private object Chanukah extends LoadNames("Chanukah") {
    val haftarahShabbos1: Haftarah.Customs = parseHaftarah(
        <haftarah book="Zechariah" fromChapter="2" fromVerse="14" toChapter="4" toVerse="7"/>)

    val haftarahShabbos2: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings" fromChapter="7" fromVerse="40" toChapter="7">
        <custom n="Common" toVerse="50"/>
        <custom n="Italki" toVerse="51"/>
      </haftarah>)
  }

  sealed class Chanukah(override val dayNumber: Int) extends WithNames with DayOf with RabbinicFestival {
    private def korbanot: Torah = Numbers.channukahKorbanot

    final override lazy val names: Names = namesWithNumber(Chanukah, dayNumber)

    final override def firstDay: Date = Chanukah1

    final override def date(year: Year): Day = year.month(Kislev).day(25)+(dayNumber-1)

    final def shabbos(weeklyReading: WeeklyReading, isRoshChodesh: Boolean): Reading = {
      val result = replaceMaftirAndHaftarah(weeklyReading.getReading,
        maftir = full(dayNumber),
        haftarah = if (dayNumber < 8) Chanukah.haftarahShabbos1 else Chanukah.haftarahShabbos2)

      if (!isRoshChodesh) result
      else result.transformTorah(torah => TorahReadings.torah7to6(torah) :+ RoshChodesh.shabbosChanukahTorah)
    }

    final def weekday(isRoshChodesh: Boolean): Reading = {
      val (common: Torah, ashkenazAndChabadTail: Torah, sefardTail: Torah) =
        if (dayNumber == 1) (
          Seq(korbanot.head),
          Seq(first(dayNumber), second(dayNumber)),
          Seq(full(dayNumber), full(dayNumber + 1))
        ) else {
          val ashkenazAndChabadTail: ChumashSpan.BookSpan = if (dayNumber == 8) korbanot.last else full(dayNumber+1)
          (
            Seq(first(dayNumber), second(dayNumber)),
            Seq(ashkenazAndChabadTail),
            Seq(full(dayNumber)+ashkenazAndChabadTail)
          )
        }

      val ashkenazAndChabad = common ++ ashkenazAndChabadTail
      val sefard = common ++ sefardTail
      require(ashkenazAndChabad.length == 3)
      require(sefard.length == 3)

      val torah: Custom.Of[Torah] = new Custom.Of[Torah](Map(
        Custom.Ashkenaz -> ashkenazAndChabad,
        Custom.Chabad -> ashkenazAndChabad,
        Custom.Sefard -> sefard
      ))

      val torahResult: Custom.Of[Torah] =
        if (!isRoshChodesh) torah
        else Custom.Of(RoshChodesh.in3aliyot :+ full(dayNumber))

      Reading(torahResult)
    }

    private def first(n: Int): ChumashSpan.BookSpan = korbanot(2*n-1)
    private def second(n: Int): ChumashSpan.BookSpan = korbanot(2*n  )
    private def full(n: Int): ChumashSpan.BookSpan = first(n)+second(n)
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

  sealed trait SpecialShabbos extends Date

  sealed trait SpecialParsha extends SpecialShabbos with MaftrirAndHaftarah {
    final def transform(reading: Reading): Reading = replaceMaftirAndHaftarah(reading, maftir, haftarah)
  }

  case object ParshasShekalim extends LoadNames("Parshas Shekalim") with SpecialParsha {
    override def date(year: Year): Day = {
      val result = Purim.date(year).month.firstDay
      if (result.isShabbos) result else result.shabbosBefore
    }

    protected override def maftir: ChumashSpan.BookSpan = Exodus.parshasShekalimMaftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="II Kings">
        <custom n="Ashkenaz, Italki, Teiman, Chabad" fromChapter="12" fromVerse="1" toVerse="17"/>
        <custom n="Sefard" fromChapter="11" fromVerse="17" toChapter="12" toVerse="17"/>
      </haftarah>)
  }

  case object ParshasZachor extends LoadNames("Parshas Zachor") with SpecialParsha {
    override def date(year: Year): Day = Purim.date(year).shabbosBefore

    protected override val maftir: ChumashSpan.BookSpan = Deuteronomy.parshasZachorMaftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel">
        <custom n="Ashkenaz, Chabad" fromChapter="15" fromVerse="2" toVerse="34"/>
        <custom n="Sefard" fromChapter="15" fromVerse="1" toVerse="34"/>
        <custom n="Teiman" fromChapter="14" fromVerse="52" toChapter="15" toVerse="33"/>
      </haftarah>)
  }

  case object ParshasParah extends LoadNames("Parshas Parah") with SpecialParsha {
    override def date(year: Year): Day = ParshasHachodesh.date(year).shabbosBefore

    protected override val maftir: ChumashSpan.BookSpan = Numbers.parshasParahMaftir

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

    protected override def maftir: ChumashSpan.BookSpan = Exodus.parshasHachodeshMaftir

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
      reading.transformR[Haftarah.Haftarah](haftarah, {
        case (custom: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah.Haftarah) =>
          if ((custom == Custom.Chabad) && !isErevPesach) readingCustom
          else readingCustom.replaceHaftarah(haftarah)
      })

    private val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="Malachi" fromChapter="3" fromVerse="4" toVerse="24"/>)
  }

  sealed trait PurimCommon extends RabbinicFestival with WeekdayReading {
    final override def weekday: Reading = Reading(Exodus.purimTorah)
  }

  case object Purim extends LoadNames("Purim") with PurimCommon {
    override def date(year: Year): Day = year.latestAdar.day(14)
  }

  case object ShushanPurim extends LoadNames("Shushan Purim") with PurimCommon {
    override def date(year: Year): Day = Purim.date(year) + 1
  }

  case object Pesach extends LoadNames("Pesach") with Festival with FirstDayOf with ShabbosAndWeekdayReading {
    final def date(year: Year): Day = year.month(Nisan).day(15)

    protected override val (shabbosTorah: Torah, weekdayTorah: Torah) = Exodus.pesach1torah

    protected override val maftir: ChumashSpan.BookSpan = Numbers.pesachMaftir

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

  case object Pesach2 extends Festival with NonFirstDayOf with WeekdayReadingSimple {
    override lazy val names: Names = namesWithNumber(Pesach, 2)

    override def firstDay: Date = Pesach

    override def dayNumber: Int = 2

    override def torah: Torah = Succos1.weekdayTorah

    protected override val maftir: ChumashSpan.BookSpan = Numbers.pesachMaftir

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

  private object PesachIntermediate extends LoadNames("Pesach Intermediate") {
    def torah(dayNumber: Int, isPesachOnChamishi: Boolean): Torah = {
      val realDayNumber: Int =
        if (isPesachOnChamishi && ((dayNumber == 4) || (dayNumber ==5))) dayNumber-1 else dayNumber
      realDayNumber match {
        case 3 => Exodus.pesach3torah
        case 4 => Exodus.pesach4torah
        case 5 => Exodus.pesach5torah
        case 6 => Numbers.pesach6torah
      }
    }

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

    protected final override def shabbosMaftir: ChumashSpan.BookSpan = Numbers.pesachEndMaftir

    protected final override def shabbosHaftarah: Haftarah.Customs = PesachIntermediate.shabbosHaftarah

    final def weekday(isPesachOnChamishi: Boolean): Reading = {
      val torah: Torah = if (dayNumber == 2) {
        val all = Pesach2.torah
        Seq(all(1), all(2), all(3) + all(4) + all(5))
      } else {
        PesachIntermediate.torah(dayNumber, isPesachOnChamishi) :+ shabbosMaftir
      }

      Reading(torah)
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

  case object Pesach7 extends LoadNames("Pesach 7") with Festival with NonFirstDayOf with ShabbosAndWeekdayReading {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 7

    protected override val (shabbosTorah: Torah, weekdayTorah: Torah) = Exodus.pesach7torah

    protected override val maftir: ChumashSpan.BookSpan = Numbers.pesachEndMaftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="II Samuel" fromChapter="22" fromVerse="1" toVerse="51"/>)
  }

  case object Pesach8 extends LoadNames("Pesach 8") with Festival with NonFirstDayOf with ShabbosAndWeekdayReading {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 8

    protected override val (shabbosTorah: Torah, weekdayTorah: Torah) = Deuteronomy.festivalEndTorah

    protected override val maftir: ChumashSpan.BookSpan = Numbers.pesachEndMaftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="Isaiah" fromChapter="10" fromVerse="32" toChapter="12" toVerse="6"/>)
  }

  case object LagBaOmer extends LoadNames("Lag Ba Omer") with Date {
    override def date(year: Year): Day = Pesach.date(year) + 33 // year.month(Iyar).day(18)
  }

  case object Shavuos extends LoadNames("Shavuos") with Festival with FirstDayOf with WeekdayReadingSimple {
    override def date(year: Year): Day = Pesach.date(year) + 50 // year.month(Sivan).day(6)

    protected override def torah: Torah = Exodus.shavuosTorah

    protected override def maftir: ChumashSpan.BookSpan = Numbers.shavuosMaftir

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

  case object Shavuos2 extends Festival with NonFirstDayOf with ShabbosAndWeekdayReading  {
    override lazy val names: Names = namesWithNumber(Shavuos, 2)

    override def firstDay: Date = Shavuos

    override def dayNumber: Int = 2

    protected override val (shabbosTorah: Torah, weekdayTorah: Torah) = Deuteronomy.festivalEndTorah

    protected override val maftir: ChumashSpan.BookSpan = Numbers.shavuosMaftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Habakkuk">
        <custom n="Ashkenaz" fromChapter="3" fromVerse="1" toVerse="19"/>
        <custom n="Sefard" fromChapter="2" fromVerse="20" toChapter="3" toVerse="19"/>
      </haftarah>)
  }

  case object FastOfTammuz extends LoadNames("Fast of Tammuz") with NonTishaBeAvFast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tammuz).day(17)
  }

  case object TishaBeAv extends LoadNames("Tisha BeAv") with Fast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Av).day(9)

    override def weekday: Reading = Reading(torah, haftarah)

    private val torah: Torah = Deuteronomy.tishaBeAvTorah

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
    new Names(withNames.names.names.map { name =>
      val numberStr = name.languageSpec.language.fold(number.toString)(_.toString(number))
      name.copy(name.name + " " + numberStr)
    })

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

  // TODO list all days with haftarah - but why?

  // TODO number of aliyot: Shabbos and Simchas Torah - 7; Yom Kippur - 6 (even on Shabbos); Yom Tov - 5;
  // Intermediate and Rosh Chodesh - 4; Chanukkah, Purim, Fast, Sheni/Chamishi and Shabbos afternoon - 3.

  final def getMorningReading(
    day: Day,
    specialDay: Option[Date],
    specialShabbos: Option[SpecialShabbos],
    weeklyReading: Option[WeeklyReading],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] = {
    if (day.isShabbos) Some(getShabbosMorningReading(day, specialDay, weeklyReading, specialShabbos))
    else getWeekdayMorningReading(day, specialDay, nextWeeklyReading, isPesachOnChamishi)
  }

  private final def getShabbosMorningReading(
    day: Day,
    specialDay: Option[Date],
    weeklyReading: Option[WeeklyReading],
    specialShabbos: Option[SpecialShabbos],
  ): Reading = {
    require(day.isShabbos)

    val normal: Reading = specialDay.map {
      case specialDay: ShabbosReading =>
        require(weeklyReading.isEmpty)
        specialDay.shabbos

      case specialDay: Chanukah =>
        require(weeklyReading.isDefined)
        specialDay.shabbos(weeklyReading.get, day.isRoshChodesh)

      case _ =>
        throw new IllegalArgumentException("Must have Shabbos reading!")
    }
      .getOrElse {
        require(weeklyReading.isDefined)
        val result = weeklyReading.get.getReading
        // TODO Chabad: on Shabbos Ki Teize (14th of Ellul) adds haftarah Reeh after the regular one.
        result
      }

    val result = specialShabbos.fold(normal) {
      case s: SpecialParsha => s.transform(normal)
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
      .orElse {
        val isSheniOrChamishi: Boolean = Set[Day.Name](Day.Name.Sheni, Day.Name.Chamishi).contains(day.name)
        if (!isSheniOrChamishi) None else Some(nextWeeklyReading.aliyot)
      }
      .orElse {
        if (!isRoshChodesh) None else Some(RoshChodesh.weekday)
      }
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

    specialReading.orElse { if (!day.isShabbos) None else Some(nextWeeklyReading.aliyot) }
  }

  private val loadNames: Seq[LoadNames] = Seq(RoshChodesh, ErevRoshChodesh,
    RoshHashanah1, FastOfGedalia, YomKippur, Succos1,
    SuccosIntermediate, SheminiAtzeres, SimchasTorah, SheminiAtzeresAndSimchasTorahInHolyLand,
    ShabbosBereishis, Chanukah, FastOfTeves,
    ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh, ShabbosHagodol,
    FastOfEster, Purim, ShushanPurim, Pesach, PesachIntermediate, Pesach7, Pesach8,
    LagBaOmer, Shavuos, FastOfTammuz, TishaBeAv
  )

  private val toNames: Map[WithName, Names] = Metadata.loadNames(loadNames, this, "SpecialDay")

  private def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs = Haftarah.parse(element, full = full)
}
