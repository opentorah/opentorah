package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish.{Day, Month, Year}
import org.opentorah.calendar.jewish.Jewish.Month.Name._
import org.opentorah.calendar.jewish.{JewishDay, JewishYear}
import org.opentorah.texts.tanach.{Custom, Haftarah, Parsha, Reading, Source, SpecialReadings, Torah}
import org.opentorah.texts.tanach.Torah.{Aliyah, Maftir}
import org.opentorah.metadata.{Metadata, Names, WithName, WithNames}
import org.opentorah.xml.{From, Parser}

import scala.xml.Elem

object SpecialDay {

  sealed class LoadNames(override val name: String) extends WithName with WithNames {
    final override def names: Names = toNames(this)
  }

  trait ParseRaw { self: WithNames =>

    protected final def parseTorah(element: Elem): Torah =
      Parser.parseDo(Torah.torahParsable.parse(From.xml("Torah", element)))
        .fromWithNumbers(this)

    protected def parseMaftir(element: Elem): Torah.Maftir =
      Parser.parseDo(Torah.maftirParsable.parse(From.xml("Maftir", element)))
        .from(this)

    protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
      Parser.parseDo(Haftarah.haftarahParsable(full).parse(From.xml("Haftarah", element)))
        .map(_.from(this), full)
  }

  trait ParseTorah extends ParseRaw { self: WithNames =>
    final lazy val torah: Torah = parseTorah(torahElement)
    protected def torahElement: Elem
  }

  trait ParseMaftir extends ParseRaw { self: WithNames =>
    final lazy val maftir: Torah.Maftir = parseMaftir(maftirElement)
    protected def maftirElement: Elem
  }

  trait ParseHaftarah extends ParseRaw { self: WithNames =>
    final lazy val haftarah: Haftarah.Customs = parseHaftarah(haftarahElement)
    protected def haftarahElement: Elem
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
    final override def weekday: Reading = Reading(torah = torah, maftir = Some(maftir), haftarah = haftarah)

    protected def torah: Torah
  }

  sealed trait ShabbosReading {
    def shabbos: Reading
  }

  sealed trait ShabbosAndWeekdayReading extends ShabbosReading with WeekdayReading with MaftrirAndHaftarah {
    final override def weekday: Reading = getReading(weekdayTorah)

    final override def shabbos: Reading = getReading(shabbosTorah)

    private def getReading(torah: Torah): Reading = Reading(torah = torah, maftir = Some(maftir), haftarah = haftarah)

    protected def shabbosTorah: Torah

    protected def weekdayTorah: Torah
  }

  sealed trait ShabbosAndWeekdayReadingParse extends ShabbosAndWeekdayReading with ParseRaw { self: WithNames =>
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

  private object FestivalEnd extends LoadNames("Festival End") with ParseRaw {
    val endShabbosTorah: Torah = parseTorah(SpecialReadings.festivalEndShabbosTorah)
    val endWeekdayTorah: Torah = Torah(endShabbosTorah.spans.drop(2))
    val sheminiAtzeresWeekdayTorah: Torah = endShabbosTorah.drop(Set(2, 3))
  }

  private object IntermediateShabbos extends LoadNames("Intermediate Shabbos") with ParseTorah {
    protected override def torahElement: Elem = SpecialReadings.intermediateShabbosTorah
  }

  sealed abstract class Intermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends FestivalOrIntermediate with NonFirstDayOf with ShabbosReading
  {
    final override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)

    final override def shabbos: Reading = Reading(
      torah = IntermediateShabbos.torah,
      maftir = Some(shabbosMaftir),
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
    with WeekdayReading with MaftirAndHaftarahTransform with ParseTorah with ParseRaw
  {
    protected override def torahElement: Elem = SpecialReadings.roshChodesh

    override val weekday: Reading = {
      val all = torah.spans
      val aliya1 = all.head+all(1)             // 1-3
      val aliya2AshkenazSefard = all(1)+all(2) // 3-5
      val aliya2Hagra = all(2)+all(3)          // 4-8
      val aliya3 = all(3)+all(4)               // 6-10
      val aliya4 = all(5)                      // 11-15
      val ashkenazSefard = Torah.aliyot(aliya1, aliya2AshkenazSefard, aliya3, aliya4).fromWithNumbers(this)
      val hagra = Torah.aliyot(aliya1, aliya2Hagra, aliya3, aliya4).fromWithNumbers(this)
      Reading(
        Custom.Ashkenaz -> ashkenazSefard,
        Custom.Sefard -> ashkenazSefard,
        Custom.Hagra -> hagra
      )
    }

    val in3aliyot: Torah = {
      val all = torah.spans
      Torah.aliyot(
        (all.head+all(1)+all(2)).from(new Source.AndNumbers(this, 1, 2)), // 1-5
        (all(3)+all(4)         ).from(new Source.AndNumber (this, 3)      ), // 6-10
         all(5)                 .from(new Source.AndNumber (this, 4)      )  // 11-15
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

    protected override val shabbosHaftarah: Haftarah.Customs =
      parseHaftarah(SpecialReadings.roshChodeshShabbosHaftarah)

    protected override val shabbosAdditionalHaftarah: Haftarah.Customs =
      parseHaftarah(full = false, element = SpecialReadings.roshChodeshShabbosAdditionalHaftarah)
  }

  case object ErevRoshChodesh extends LoadNames("Erev Rosh Chodesh") with MaftirAndHaftarahTransform
    with ParseRaw
  {
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

    protected override val shabbosHaftarah: Haftarah.Customs =
      parseHaftarah(SpecialReadings.erevRoshChodeshShabbosHaftarah)

    protected override val shabbosAdditionalHaftarah: Haftarah.Customs =
      parseHaftarah(full = false, element = SpecialReadings.erevRoshChodeshShabbosAdditionalHaftarah)
  }

  private object Fast extends LoadNames("Public Fast") with ParseRaw {
    val torah: Torah = Torah.aliyot(
      parseTorah(SpecialReadings.fastAfternoonTorahPart1).spans.head, // Exodus 32:11-14
      IntermediateShabbos.torah.spans(3),                             // Exodus 34:1-3
      IntermediateShabbos.torah.spans(4)                              // Exodus 34:4-10
    ).fromWithNumbers(this)

    val defaultAfternoonHaftarah: Haftarah.Customs =
      parseHaftarah(full = false, element = SpecialReadings.fastDefaultAfternoonHaftarah)
  }

  sealed trait Fast extends Date with WeekdayReading with AfternoonReading {
    final override def afternoon: Reading = {
      val torah: Torah = Fast.torah.fromWithNumbers(this)
      val haftarah: Haftarah.Customs =
        afternoonHaftarahExceptions.fold(Fast.defaultAfternoonHaftarah) { afternoonHaftarahExceptions =>
          Fast.defaultAfternoonHaftarah ++ afternoonHaftarahExceptions }

      new Reading(
        customs = haftarah.lift { case (_: Custom, haftarah: Option[Haftarah]) =>
          haftarah.fold(Reading.ReadingCustom(torah, None)) { haftarah: Haftarah =>
            Reading.ReadingCustom(
              torah = Torah(torah.spans),
              maftirAndHaftarah = Some(Reading.MaftirAndHaftarah(None, haftarah))
            )
          }
        }.customs
      )
    }

    protected val afternoonHaftarahExceptions: Option[Haftarah.Customs] = None
  }

  sealed trait NonTishaBeAvFast extends Fast {
    final override def weekday: Reading = Reading(Fast.torah.fromWithNumbers(this))
  }

  case object RoshHashanah1 extends LoadNames("Rosh Hashanah") with Festival
    with ShabbosAndWeekdayReadingParse with ParseMaftir with ParseHaftarah
  {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(1)

    protected override def shabbosTorahElement: Elem = SpecialReadings.roshHashana1torah
    protected override def dropOnWeekday: Set[Int] = Set(3, 5)

    protected override def maftirElement: Elem = SpecialReadings.roshHashanahMaftir
    protected override def haftarahElement: Elem = SpecialReadings.roshHashanah1haftarah
  }

  case object RoshHashanah2 extends Festival with NonFirstDayOf with WeekdayReadingSimple
    with ParseTorah with ParseHaftarah
  {
    override def firstDay: Date = RoshHashanah1

    override def dayNumber: Int = 2

    override lazy val names: Names = namesWithNumber(RoshHashanah1, 2)

    protected override def torahElement: Elem = SpecialReadings.roshHashanah2torah
    protected override def maftir: Maftir = RoshHashanah1.maftir
    protected override def haftarahElement: Elem = SpecialReadings.roshHashanah2haftarah
  }

  case object FastOfGedalia extends LoadNames("Fast of Gedalia")
    with NonTishaBeAvFast with PostponeOnShabbos with ParseRaw
  {
    final override def date(year: Year): Day = year.month(Tishrei).day(3)

    protected override val afternoonHaftarahExceptions: Option[Haftarah.Customs] =
      Some(parseHaftarah(full = false, element = SpecialReadings.fasOfGedaliaAfternoonHaftarahExceptions))
  }

  case object YomKippur extends LoadNames("Yom Kippur") with Festival with ShabbosAndWeekdayReadingParse
    with AfternoonReading with ParseMaftir with ParseHaftarah
  {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(10)

    protected override def shabbosTorahElement: Elem = SpecialReadings.yomKippurTorah
    protected override val dropOnWeekday: Set[Int] = Set(2)

    protected override def maftirElement: Elem = SpecialReadings.yomKippurMaftir

    protected override def haftarahElement: Elem = SpecialReadings.yomKippurHaftarah

    override lazy val afternoon: Reading = Reading(torah = afternoonTorah, maftir = None, haftarah = afternoonHaftarah)

    private val afternoonTorah: Torah = parseTorah(SpecialReadings.yomKippurAfternoonTorah)

    private val afternoonHaftarah: Haftarah.Customs =
      parseHaftarah(SpecialReadings.yomKippurAfternoonHaftarah)
  }

  case object Succos1 extends LoadNames("Succos") with Festival with FirstDayOf
    with ShabbosAndWeekdayReadingParse with ParseHaftarah
  {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(15)

    override def shabbosTorahElement: Elem = SpecialReadings.succos1and2torah
    override val dropOnWeekday: Set[Int] = Set(2, 4)
    protected override def haftarahElement: Elem = SpecialReadings.succos1haftarah
    override def maftir: Maftir = SuccosIntermediate.korbanot.head
  }

  case object Succos2 extends Festival with NonFirstDayOf with ShabbosAndWeekdayReading with ParseHaftarah {
    override def names: Names = namesWithNumber(Succos1, 2)

    override def firstDay: Date = Succos1

    override def dayNumber: Int = 2

    protected override def shabbosTorah: Torah = Succos1.shabbosTorah
    protected override def weekdayTorah: Torah = Succos1.weekdayTorah
    protected override def maftir: Maftir = Succos1.maftir
    protected override def haftarahElement: Elem = SpecialReadings.succos2haftarah
  }

  private object SuccosIntermediate extends LoadNames("Succos Intermediate") with ParseRaw {
    val korbanot: Seq[Torah.Fragment] = parseTorah(SpecialReadings.succosKorbanot).spans
    val shabbosHaftarah: Haftarah.Customs = parseHaftarah(SpecialReadings.succosIntermediateShabbosHaftarah)
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

      Reading(
        Custom.Ashkenaz -> ashkenazAndChabad,
        Custom.Chabad -> ashkenazAndChabad,
        Custom.Sefard -> Torah.aliyot(sefard, sefard, sefard, sefard)
      )
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
    with Festival with NonFirstDayOf with ShabbosAndWeekdayReading with ParseHaftarah
  {
    override def firstDay: Date = Succos1
    override def dayNumber: Int = 8

    protected override def shabbosTorah: Torah = FestivalEnd.endShabbosTorah
    protected override def weekdayTorah: Torah = FestivalEnd.sheminiAtzeresWeekdayTorah
    override val maftir: Maftir = SuccosIntermediate.korbanot.last
    protected override def haftarahElement: Elem = SpecialReadings.sheminiAtzeresHaftarah
  }

  sealed trait SimchasTorahCommon extends Festival with NonFirstDayOf with WeekdayReadingSimple with ParseHaftarah {
    final override def firstDay: Date = Succos1

    protected override def maftir: Maftir = SheminiAtzeres.maftir
    protected override def haftarahElement: Elem = SpecialReadings.simchasTorahHaftarah
  }

  case object SimchasTorah extends LoadNames("Simchas Torah") with SimchasTorahCommon with ParseRaw {
    override def dayNumber: Int = 9

    val chassanBereishis: Torah.Fragment = parseTorah(SpecialReadings.chassanBereishis).spans.head

    override val torah: Torah = Parsha.VezosHaberachah.days.common
      .fromWithNumbers(SimchasTorah)
      .to6withLast(chassanBereishis)
  }

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends LoadNames("Shemini Atzeres and Simchas Torah")
    with SimchasTorahCommon with ShabbosReading
  {
    override def dayNumber: Int = 8

    protected override def torah: Torah = SimchasTorah.torah

    override def shabbos: Reading = weekday
  }

  case object ShabbosBereishis extends LoadNames("Shabbos Bereishis") with Date {
    override def date(year: Year): Day = SimchasTorah.date(year).shabbosAfter
  }

  private object Chanukah extends LoadNames("Chanukah") with ParseRaw {
    val day1Cohen: Torah = parseTorah(SpecialReadings.chanukahFirst)

    val korbanot: Seq[Torah.Fragment] = parseTorah(SpecialReadings.chanukahKorbanotSpans).spans
    def first(n: Int): Aliyah = korbanot(2*(n-1))
    def second(n: Int): Aliyah = korbanot(2*(n-1)+1)
    val zos: Torah.Fragment = korbanot.last

    val haftarahShabbos1: Haftarah.Customs = parseHaftarah(SpecialReadings.chanukahShabbos1Haftarah)
    val haftarahShabbos2: Haftarah.Customs = parseHaftarah(SpecialReadings.chanukahShabbos2Haftarah)
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

      if (isRoshChodesh) Reading(RoshChodesh.in3aliyot :+ full(dayNumber).from(this))
      else Reading(
        Custom.Ashkenaz -> Torah(ashkenazAndChabad).from(this),
        Custom.Chabad -> Torah(ashkenazAndChabad).from(this),
        Custom.Sefard -> Torah(sefard).from(this)
      )
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

  sealed trait SpecialShabbos extends Date with ParseRaw with ParseHaftarah

  sealed trait SpecialParsha extends SpecialShabbos with MaftrirAndHaftarah with ParseMaftir {
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

    protected override def maftirElement: Elem = SpecialReadings.parshasShekalimMaftir
    protected override def haftarahElement: Elem = SpecialReadings.parshasShekalimHaftarah
  }

  case object ParshasZachor extends LoadNames("Parshas Zachor") with SpecialParsha {
    override def date(year: Year): Day = Purim.date(year).shabbosBefore

    protected override def maftirElement: Elem = SpecialReadings.parshasZachorMaftir
    protected override def haftarahElement: Elem = SpecialReadings.parshasZachorHaftarah
  }

  case object ParshasParah extends LoadNames("Parshas Parah") with SpecialParsha {
    override def date(year: Year): Day = ParshasHachodesh.date(year).shabbosBefore

    protected override def maftirElement: Elem = SpecialReadings.parshasParahMaftir
    protected override def haftarahElement: Elem = SpecialReadings.parshasParahHaftarah
  }

  case object ParshasHachodesh extends LoadNames("Parshas Hachodesh") with SpecialParsha {
    override def date(year: Year): Day = {
      val result = year.month(Nisan).firstDay
      if (result.isShabbos) result else result.shabbosBefore
    }

    protected override def maftirElement: Elem = SpecialReadings.parshasHachodeshMaftir
    protected override def haftarahElement: Elem = SpecialReadings.parshasHachodeshHaftarah
  }

  case object ShabbosHagodol extends LoadNames("Shabbos Hagodol") with SpecialShabbos {
    override def date(year: Year): Day = Pesach.date(year).shabbosBefore

    def transform(isErevPesach: Boolean, reading: Reading): Reading =
      reading.transform[Haftarah](haftarah, {
        case (custom: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
          if ((custom == Custom.Chabad) && !isErevPesach) readingCustom
          else readingCustom.replaceHaftarah(haftarah)
      })

    protected override def haftarahElement: Elem = SpecialReadings.shabbosHagodolHaftarah
  }

  sealed trait PurimCommon extends RabbinicFestival with WeekdayReading

  case object Purim extends LoadNames("Purim") with PurimCommon with ParseTorah {
    override def date(year: Year): Day = year.latestAdar.day(14)

    protected override def torahElement: Elem = SpecialReadings.purimTorah

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

    private val shushanPurimShabbosMaftir: Torah.Aliyah =
      Torah.merge(Purim.torah.spans.take(3)).from(this)
  }

  case object Pesach extends LoadNames("Pesach") with Festival with FirstDayOf
    with ShabbosAndWeekdayReadingParse with ParseMaftir with ParseHaftarah
  {
    final def date(year: Year): Day = year.month(Nisan).day(15)

    protected override def shabbosTorahElement: Elem = SpecialReadings.pesach1torah
    protected override val dropOnWeekday: Set[Int] = Set(4, 7)
    protected override def maftirElement: Elem = SpecialReadings.pesachMaftir
    protected override def haftarahElement: Elem = SpecialReadings.pesachHaftarah
  }

  case object Pesach2 extends Festival with NonFirstDayOf with WeekdayReadingSimple with ParseHaftarah {
    override lazy val names: Names = namesWithNumber(Pesach, 2)

    override def firstDay: Date = Pesach

    override def dayNumber: Int = 2

    override def torah: Torah = Succos1.weekdayTorah
    protected override val maftir: Maftir = Pesach.maftir
    protected override def haftarahElement: Elem = SpecialReadings.pesach2Haftarah
  }

  private object PesachIntermediate extends LoadNames("Pesach Intermediate") with ParseRaw {
    def first5(realDayNumber: Int): Torah = realDayNumber match {
      case 2 => day2
      case 3 => day3
      case 4 => day4
      case 5 => day5
      case 6 => day6
    }

    private val day2: Torah = Pesach2.torah.drop(Set(4, 5))
    private val day3: Torah = parseTorah(SpecialReadings.pesach3torah)
    private val day4: Torah = parseTorah(SpecialReadings.pesach4torah)
    private val day5: Torah = {
      val all = IntermediateShabbos.torah.spans
      Torah.aliyot( // Exodus.pesach5torah(this)
        all(3),         // Exodus 34:1-3
        all(4)+all(5),  // Exodus 34:4-17
        all(6)          // Exodus 34:18-26
      )
    }
    private val day6: Torah = parseTorah(SpecialReadings.pesach6torah)

    val shabbosHaftarah: Haftarah.Customs = parseHaftarah(SpecialReadings.pesachIntermediateShabbosHaftarah)
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

  case object Pesach7 extends LoadNames("Pesach 7") with Festival with NonFirstDayOf
    with ShabbosAndWeekdayReadingParse with ParseMaftir with ParseHaftarah
  {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 7

    protected override def shabbosTorahElement: Elem = SpecialReadings.pesach7torah
    protected override val dropOnWeekday: Set[Int] = Set(2, 4)
    protected override def maftirElement: Elem = SpecialReadings.pesachEndMaftir
    protected override def haftarahElement: Elem = SpecialReadings.pesach7Hhaftarah
  }

  case object Pesach8 extends LoadNames("Pesach 8")
    with Festival with NonFirstDayOf with ShabbosAndWeekdayReading with ParseHaftarah
  {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 8

    protected override val shabbosTorah: Torah = FestivalEnd.endShabbosTorah
    protected override val weekdayTorah: Torah = FestivalEnd.endWeekdayTorah
    protected override val maftir: Maftir = Pesach7.maftir
    protected override def haftarahElement: Elem = SpecialReadings.pesach8Haftarah
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
    with Festival with FirstDayOf with WeekdayReadingSimple with ParseTorah with ParseMaftir with ParseHaftarah
  {
    override def date(year: Year): Day = Pesach.date(year) + 50

    protected override def torahElement: Elem = SpecialReadings.shavuosTorah
    protected override def maftirElement: Elem = SpecialReadings.shavuosMaftir
    protected override val haftarahElement: Elem = SpecialReadings.shavuosHaftarah
  }

  case object Shavuos2 extends Festival with NonFirstDayOf with ShabbosAndWeekdayReading with ParseHaftarah {
    override lazy val names: Names = namesWithNumber(Shavuos, 2)

    override def firstDay: Date = Shavuos

    override def dayNumber: Int = 2

    protected override val shabbosTorah: Torah = FestivalEnd.endShabbosTorah
    protected override val weekdayTorah: Torah = FestivalEnd.endWeekdayTorah
    protected override val maftir: Maftir = Shavuos.maftir
    protected override val haftarahElement: Elem = SpecialReadings.shavuos2Haftarah
  }

  case object FastOfTammuz extends LoadNames("Fast of Tammuz") with NonTishaBeAvFast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tammuz).day(17)
  }

  case object TishaBeAv extends LoadNames("Tisha BeAv") with Fast with PostponeOnShabbos
    with ParseTorah with ParseHaftarah
  {
    override def date(year: Year): Day = year.month(Av).day(9)

    override def weekday: Reading = Reading(torah = torah, maftir = None, haftarah = haftarah)

    protected override def torahElement: Elem = SpecialReadings.tishaBeAvTorah
    protected override def haftarahElement: Elem = SpecialReadings.tishaBeAvHaftarah
  }

  private def namesWithNumber(withNames: WithNames, number: Int): Names = withNames.names.withNumber(number)

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

    result.fold(require(numAliyot == 0)) { _.torah.customs.values.foreach { torah => require(torah.length == numAliyot) }}

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

    result.foreach { _.torah.customs.values.foreach { torah => require(torah.length == 3) }}

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

  private val toNames: Map[WithName, Names] = Metadata.loadNames(
    keys = loadNames,
    from = From.resource(this, "SpecialDay")
  )
}
