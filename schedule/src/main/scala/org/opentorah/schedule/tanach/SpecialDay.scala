package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish.{Day, Month, Year}
import org.opentorah.calendar.jewish.Jewish.Month.Name._
import org.opentorah.calendar.jewish.{JewishDay, JewishYear}
import org.opentorah.texts.tanach.{Custom, Haftarah, Parsha, Reading, SpecialReadings, Torah}
import org.opentorah.texts.tanach.Torah.{Aliyah, Maftir}
import org.opentorah.metadata.{Metadata, Names, WithName, WithNames}
import org.opentorah.xml.From

object SpecialDay {

  def fromDay(withNames: WithNames, torah: Torah): Torah = torah.fromWithNumbers(withNames)

  def fromDay(withNames: WithNames, maftir: Maftir): Maftir = maftir.from(withNames)

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

  sealed trait FestivalOrIntermediate extends Date

  sealed trait Festival extends FestivalOrIntermediate

  private object FestivalEnd extends LoadNames("Festival End")

  private object IntermediateShabbos extends LoadNames("Intermediate Shabbos")

  sealed abstract class Intermediate(val intermediateDayNumber: Int, val inHolyLand: Boolean)
    extends FestivalOrIntermediate with NonFirstDayOf
  {
    final override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)
  }

  sealed trait RabbinicFestival extends Date

  case object RoshChodesh extends LoadNames("Rosh Chodesh")

  case object ErevRoshChodesh extends LoadNames("Erev Rosh Chodesh")

  private object Fast extends LoadNames("Public Fast")

  sealed trait Fast extends Date

  case object RoshHashanah1 extends LoadNames("Rosh Hashanah") with Festival {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(1)
  }

  case object RoshHashanah2 extends Festival with NonFirstDayOf {
    override def firstDay: Date = RoshHashanah1
    override def dayNumber: Int = 2
    override lazy val names: Names = namesWithNumber(RoshHashanah1, 2)
  }

  case object FastOfGedalia extends LoadNames("Fast of Gedalia") with Fast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tishrei).day(3)
  }

  case object YomKippur extends LoadNames("Yom Kippur") with Festival {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(10)
  }

  case object Succos1 extends LoadNames("Succos") with Festival with FirstDayOf {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(15)
  }

  case object Succos2 extends Festival with NonFirstDayOf {
    override def names: Names = namesWithNumber(Succos1, 2)
    override def firstDay: Date = Succos1
    override def dayNumber: Int = 2
  }

  private object SuccosIntermediate extends LoadNames("Succos Intermediate")

  sealed class SuccosIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand)
  {
    final override lazy val names: Names = namesWithNumber(SuccosIntermediate, intermediateDayNumber)
    override def firstDay: Date = Succos1
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

  case object SheminiAtzeres extends LoadNames("Shemini Atzeres") with Festival with NonFirstDayOf {
    override def firstDay: Date = Succos1
    override def dayNumber: Int = 8
  }

  case object SimchasTorah extends LoadNames("Simchas Torah") with Festival with NonFirstDayOf {
    final override def firstDay: Date = Succos1
    override def dayNumber: Int = 9
  }

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends LoadNames("Shemini Atzeres and Simchas Torah")
    with Festival with NonFirstDayOf
  {
    final override def firstDay: Date = Succos1
    override def dayNumber: Int = 8
  }

  case object ShabbosBereishis extends LoadNames("Shabbos Bereishis") with Date {
    override def date(year: Year): Day = SimchasTorah.date(year).shabbosAfter
  }

  private object Chanukah extends LoadNames("Chanukah")

  sealed class Chanukah(override val dayNumber: Int) extends WithNames with DayOf with RabbinicFestival {
    final override lazy val names: Names = namesWithNumber(Chanukah, dayNumber)
    final override def firstDay: Date = Chanukah1
    final override def date(year: Year): Day = year.month(Kislev).day(25)+(dayNumber-1)

    private def day1Cohen: Torah = SpecialReadings.Chanukah.first
    private def korbanot: Seq[Torah.Fragment] = SpecialReadings.Chanukah.korbanot
    private def first(n: Int): Aliyah = korbanot(2*(n-1))
    private def second(n: Int): Aliyah = korbanot(2*(n-1)+1)
    private def zos: Torah.Fragment = korbanot.last
    private def split(n: Int): Seq[Aliyah] = Seq(first(n), second(n))
    private def full(n: Int): Aliyah = first(n)+second(n)

    final def shabbos(weeklyReading: WeeklyReading, isRoshChodesh: Boolean): Reading = {
      val result = SpecialReadings.replaceMaftirAndHaftarah(weeklyReading.getMorningReading,
        maftir = fromDay(this, full(dayNumber)),
        haftarah = if (dayNumber < 8) SpecialReadings.Chanukah.shabbos1Haftarah
                   else SpecialReadings.Chanukah.shabbos2Haftarah)

      if (!isRoshChodesh) result
      else SpecialReadings.RoshChodesh.addShabbosMaftirAs7thAliyah(result, RoshChodesh)
    }

    final def weekday(isRoshChodesh: Boolean): Reading = {
      val (
        ashkenazAndChabad: Seq[Aliyah],
        sefard: Seq[Aliyah]
      ) = if (dayNumber == 1) {
        val day1CohenAshkenazAndChabad: Torah.Fragment = day1Cohen.spans(1)
        val day1CohenSefard: Torah.Fragment = day1Cohen.spans.head + day1CohenAshkenazAndChabad
        (
          day1CohenAshkenazAndChabad +: split(dayNumber),
          day1CohenSefard +: split(dayNumber)
        )
      } else if (dayNumber != 8) (
        split(dayNumber) :+ full(dayNumber+1),
        split(dayNumber) :+ full(dayNumber)
      ) else (
        split(dayNumber) :+ zos,
        split(dayNumber) :+ (full(dayNumber) + zos)
      )

      require(ashkenazAndChabad.length == 3)
      require(sefard.length == 3)

      if (isRoshChodesh) Reading(
        SpecialReadings.RoshChodesh.in3aliyot(RoshChodesh) :+ fromDay(this, full(dayNumber))
      ) else Reading(
        Custom.Ashkenaz -> fromDay(this, Torah(ashkenazAndChabad)),
        Custom.Chabad -> fromDay(this, Torah(ashkenazAndChabad)),
        Custom.Sefard -> fromDay(this, Torah(sefard))
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

  case object FastOfTeves extends LoadNames("Fast of 10th of Teves") with Fast {
    override def date(year: Year): Day = year.month(Teves).day(10)
  }

  case object FastOfEster extends LoadNames("Fast of Ester") with Fast {
    override def date(year: Year): Day = Purim.date(year)-1

    protected override def correctDate(result: Day): Day =
      // If on Friday or Saturday - move to Thursday
      if (result.isShabbos) result-2 else
      if (result.next.isShabbos) result-1 else
        result
  }

  sealed trait SpecialShabbos extends Date

  sealed trait SpecialParsha extends SpecialShabbos

  case object ParshasShekalim extends LoadNames("Parshas Shekalim") with SpecialParsha {
    override def date(year: Year): Day = {
      val result = Purim.date(year).month.firstDay
      if (result.isShabbos) result else result.shabbosBefore
    }
  }

  case object ParshasZachor extends LoadNames("Parshas Zachor") with SpecialParsha {
    override def date(year: Year): Day = Purim.date(year).shabbosBefore
  }

  case object ParshasParah extends LoadNames("Parshas Parah") with SpecialParsha {
    override def date(year: Year): Day = ParshasHachodesh.date(year).shabbosBefore
  }

  case object ParshasHachodesh extends LoadNames("Parshas Hachodesh") with SpecialParsha {
    override def date(year: Year): Day = {
      val result = year.month(Nisan).firstDay
      if (result.isShabbos) result else result.shabbosBefore
    }
  }

  case object ShabbosHagodol extends LoadNames("Shabbos Hagodol") with SpecialShabbos {
    override def date(year: Year): Day = Pesach1.date(year).shabbosBefore
  }

  case object Purim extends LoadNames("Purim") with RabbinicFestival {
    override def date(year: Year): Day = year.latestAdar.day(14)
  }

  case object ShushanPurim extends LoadNames("Shushan Purim") with RabbinicFestival {
    override def date(year: Year): Day = Purim.date(year) + 1
  }

  case object Pesach1 extends LoadNames("Pesach") with Festival with FirstDayOf {
    def date(year: Year): Day = year.month(Nisan).day(15)
  }

  case object Pesach2 extends Festival with NonFirstDayOf {
    override lazy val names: Names = namesWithNumber(Pesach1, 2)
    override def firstDay: Date = Pesach1
    override def dayNumber: Int = 2
  }

  private object PesachIntermediate extends LoadNames("Pesach Intermediate")

  sealed class PesachIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand)
  {
    final override lazy val names: Names = namesWithNumber(PesachIntermediate, intermediateDayNumber)
    final override def firstDay: Date = Pesach1
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

  case object Pesach7 extends LoadNames("Pesach 7") with Festival with NonFirstDayOf {
    override def firstDay: Date = Pesach1
    override def dayNumber: Int = 7
  }

  case object Pesach8 extends LoadNames("Pesach 8") with Festival with NonFirstDayOf {
    override def firstDay: Date = Pesach1
    override def dayNumber: Int = 8
  }

  case class Omer(number: Int) extends WithNames {
    override def names: Names = namesWithNumber(Omer, number)
  }

  object Omer extends LoadNames("Omer") {
    def dayOf(day: Day): Option[Omer] = {
      val year = day.year
      val pesach = Pesach1.date(year)
      val shavous = Shavuos1.date(year)
      if ((day <= pesach) || (day >= shavous)) None else Some(Omer(day - pesach))
    }
  }

  case object LagBaOmer extends LoadNames("Lag Ba Omer") with Date {
    override def date(year: Year): Day = Pesach1.date(year) + 33
  }

  case object Shavuos1 extends LoadNames("Shavuos") with Festival with FirstDayOf {
    override def date(year: Year): Day = Pesach1.date(year) + 50
  }

  case object Shavuos2 extends Festival with NonFirstDayOf {
    override lazy val names: Names = namesWithNumber(Shavuos1, 2)
    override def firstDay: Date = Shavuos1
    override def dayNumber: Int = 2
  }

  case object FastOfTammuz extends LoadNames("Fast of Tammuz") with Fast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tammuz).day(17)
  }

  case object TishaBeAv extends LoadNames("Tisha BeAv") with Fast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Av).day(9)
  }

  private def namesWithNumber(withNames: WithNames, number: Int): Names = withNames.names.withNumber(number)

  val festivals: Set[FestivalOrIntermediate] = Set(
    RoshHashanah1, RoshHashanah2,
    YomKippur,
    Succos1, Succos2,
    SuccosIntermediate1, SuccosIntermediate2, SuccosIntermediate3, SuccosIntermediate4,
    HoshanahRabbah, SheminiAtzeres, SimchasTorah,
    Pesach1, Pesach2, PesachIntermediate1, PesachIntermediate2, PesachIntermediate3, PesachIntermediate4, Pesach7, Pesach8,
    Shavuos1, Shavuos2
  )

  val festivalsInHolyLand: Set[FestivalOrIntermediate] = Set(
    RoshHashanah1, RoshHashanah2,
    YomKippur,
    Succos1,
    SuccosIntermediate1InHolyLand, SuccosIntermediate2InHolyLand, SuccosIntermediate3InHolyLand,
    SuccosIntermediate4InHolyLand, SuccosIntermediate5InHolyLand,
    HoshanahRabbahInHolyLand, SheminiAtzeresAndSimchasTorahInHolyLand,
    Pesach1, PesachIntermediate1InHolyLand, PesachIntermediate2InHolyLand, PesachIntermediate3InHolyLand,
    PesachIntermediate4InHolyLand, PesachIntermediate5InHolyLand, Pesach7,
    Shavuos1
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
      case specialDay: Chanukah =>
        require(weeklyReading.isDefined)
        specialDay.shabbos(weeklyReading.get, isRoshChodesh)

      case ShushanPurim =>
        require(weeklyReading.isDefined)
        // TODO split WeeklyReading from its calculator; move it into texts; push this into SpecialReadings:
        SpecialReadings.replaceMaftirAndHaftarah(
          weeklyReading.get.getMorningReading,
          maftir = fromDay(ShushanPurim, SpecialReadings.ShushanPurim.shabbosMaftir),
          haftarah = SpecialReadings.ParshasZachor.haftarah // TODO from?
        )

      // TODO for all below:  require(weeklyReading.isEmpty)

      case day: SuccosIntermediate => SpecialReadings.SuccosIntermediate.shabbos(day, day.intermediateDayNumber, day.inHolyLand)

      case day =>
        val reading: SpecialReadings.ShabbosReading = day match {
          case _: PesachIntermediate => SpecialReadings.PesachIntermediate
          case RoshHashanah1 => SpecialReadings.RoshHashanah1
          case YomKippur => SpecialReadings.YomKippur
          case Succos1 => SpecialReadings.Succos1
          case Succos2 => SpecialReadings.Succos2
          case SheminiAtzeres => SpecialReadings.SheminiAtzeres
          case SheminiAtzeresAndSimchasTorahInHolyLand => SpecialReadings.SheminiAtzeresAndSimchasTorahInHolyLand
          case Pesach1 => SpecialReadings.Pesach1
          case Pesach7 => SpecialReadings.Pesach7
          case Pesach8 => SpecialReadings.Pesach8
          case Shavuos2 => SpecialReadings.Shavuos2
          case _ => throw new IllegalArgumentException("Must have Shabbos reading!")
        }
        reading.shabbos(day)
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
      case ShabbosHagodol =>
        SpecialReadings.ShabbosHagodol.transform(
          day = ShabbosHagodol,
          isErevPesach = day.next == Pesach1.date(day.year),
          reading = normal
        )

      case specialParsha: SpecialParsha =>
        val reading = specialParsha match {
          case ParshasShekalim  => SpecialReadings.ParshasShekalim
          case ParshasZachor    => SpecialReadings.ParshasZachor
          case ParshasParah     => SpecialReadings.ParshasParah
          case ParshasHachodesh => SpecialReadings.ParshasHachodesh
        }
        reading.transform(normal, specialParsha, if (isRoshChodesh) Some(RoshChodesh) else None) // TODO name of the month?
    }

    val roshChodeshOf: Option[Month.Name] = {
      val result = day.roshChodeshOf
      // We do not mention Rosh Chodesh on Rosh Hashanah
      if (result.contains(Tishrei)) None else result
    }

    val isSpecialShabbos: Boolean = specialShabbos.isDefined

    val correctedForRoshChodesh = roshChodeshOf
      .fold(result) { month => SpecialReadings.RoshChodesh.correct(
        day = RoshChodesh,
        isSpecialShabbos,
        isMonthTevesAv = (month == Teves) || (month == Av),
        isMonthElul = month == Elul,
        result
      )}

    day.next.roshChodeshOf
      .fold(correctedForRoshChodesh) { month => SpecialReadings.ErevRoshChodesh.correct(
        day = ErevRoshChodesh,
        isSpecialShabbos,
        isRoshChodesh = roshChodeshOf.isDefined,
        isMonthTevesAvElul = (month == Teves) || (month == Av) || (month == Elul),
        correctedForRoshChodesh
      )}
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
      case day: SuccosIntermediate => SpecialReadings.SuccosIntermediate.weekday(day, day.intermediateDayNumber, day.inHolyLand)
      case specialDay: Chanukah => specialDay.weekday(isRoshChodesh)
      case day: PesachIntermediate => SpecialReadings.PesachIntermediate.weekday(day, isPesachOnChamishi, day.dayNumber)

      case day =>
        val reading: SpecialReadings.WeekdayReading = day match {
          case RoshHashanah1 => SpecialReadings.RoshHashanah1
          case RoshHashanah2 => SpecialReadings.RoshHashanah2
          case YomKippur => SpecialReadings.YomKippur
          case Succos1 => SpecialReadings.Succos1
          case Succos2 => SpecialReadings.Succos2
          case SheminiAtzeres => SpecialReadings.SheminiAtzeres
          case SimchasTorah => SpecialReadings.SimchasTorah
          case SheminiAtzeresAndSimchasTorahInHolyLand => SpecialReadings.SheminiAtzeresAndSimchasTorahInHolyLand
          case Purim => SpecialReadings.Purim
          case ShushanPurim => SpecialReadings.ShushanPurim
          case Pesach1 => SpecialReadings.Pesach1
          case Pesach2 => SpecialReadings.Pesach2
          case Pesach7 => SpecialReadings.Pesach7
          case Pesach8 => SpecialReadings.Pesach8
          case Shavuos1 => SpecialReadings.Shavuos1
          case Shavuos2 => SpecialReadings.Shavuos2
          case _ => throw new IllegalArgumentException("Must have weekday reading!")
        }
        reading.weekday(day)
    }

    specialReading
      .orElse { if (!isRoshChodesh) None else Some(SpecialReadings.RoshChodesh.weekday(RoshChodesh)) }
      .orElse { if (!sheniAndChamishi.contains(day.name)) None else Some(nextWeeklyReading.getAfternoonReading) }
  }

  // On Festival that falls on Shabbos, afternoon reading is that of the Shabbos - except on Yom Kippur.
  def getAfternoonReading(
    day: Day,
    specialDay: Option[Date],
    nextWeeklyReading: WeeklyReading
  ): Option[Reading] = {
    val specialReading: Option[Reading] = specialDay flatMap {
      case YomKippur => Some(SpecialReadings.YomKippur.afternoon(YomKippur))

      case fast: Fast =>
        val reading: SpecialReadings.Fast = fast match {
          case FastOfGedalia => SpecialReadings.FastOfGedalia
          case FastOfTeves   => SpecialReadings.FastOfTeves
          case FastOfEster   => SpecialReadings.FastOfEster
          case FastOfTammuz  => SpecialReadings.FastOfTammuz
          case TishaBeAv     => SpecialReadings.TishaBeAv
        }
        Some(reading.afternoon(fast))

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
    FastOfEster, Purim, ShushanPurim, Pesach1, PesachIntermediate, Pesach7, Pesach8,
    Omer, LagBaOmer, Shavuos1, FastOfTammuz, TishaBeAv
  )

  private val toNames: Map[WithName, Names] = Metadata.loadNames(
    keys = loadNames,
    from = From.resource(this, "SpecialDay")
  )
}
