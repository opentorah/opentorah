package org.opentorah.calendar.jewish

import org.opentorah.metadata.{NamedCompanion, Named, Names}
import Jewish.{Day, Year}
import Jewish.Month._

sealed trait SpecialDay extends Named {
  def date(year: Year): Day
  final def correctedDate(year: Year): Day = correctDate(date(year))
  protected def correctDate(date: Day): Day = date
}

object SpecialDay extends NamedCompanion {

  type Key = LoadNames

  sealed class LoadNames(override val name: String) extends Named {
    final override def names: Names = toNames(this)
  }

  sealed trait PostponeOnShabbos extends SpecialDay {
    final override protected def correctDate(result: Day): Day = if (result.isShabbos) result+1 else result
  }

  sealed trait DayOf extends SpecialDay {
    def firstDay: SpecialDay
    def dayNumber: Int
  }

  sealed trait FirstDayOf extends DayOf {
    final override def firstDay: SpecialDay = this
    final override def dayNumber: Int = 1
  }

  sealed trait NonFirstDayOf extends DayOf {
    final override def date(year: Year): Day = firstDay.date(year) + (dayNumber-1)
  }

  sealed trait FestivalOrIntermediate extends SpecialDay

  sealed trait Festival extends FestivalOrIntermediate

  private object FestivalEnd extends LoadNames("Festival End")
  private object IntermediateShabbos extends LoadNames("Intermediate Shabbos")
  private object Fast extends LoadNames("Public Fast")

  sealed abstract class Intermediate(val intermediateDayNumber: Int, val inHolyLand: Boolean)
    extends FestivalOrIntermediate with NonFirstDayOf
  {
    final override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)
  }

  sealed trait RabbinicFestival extends SpecialDay

  case object RoshChodesh extends LoadNames("Rosh Chodesh")

  case object ErevRoshChodesh extends LoadNames("Erev Rosh Chodesh")

  sealed trait Fast extends SpecialDay

  case object RoshHashanah1 extends LoadNames("Rosh Hashanah") with Festival {
    override def date(year: Jewish.Year): Jewish.Day = year.month(Tishrei).day(1)
  }

  case object RoshHashanah2 extends Festival with NonFirstDayOf {
    override def firstDay: SpecialDay = RoshHashanah1
    override def dayNumber: Int = 2
    override lazy val names: Names = namesWithNumber(RoshHashanah1, 2)
  }

  case object FastOfGedalia extends LoadNames("Fast of Gedalia") with Fast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tishrei).day(3)
  }

  case object YomKippur extends LoadNames("Yom Kippur") with Festival {
    override def date(year: Jewish.Year): Jewish.Day = year.month(Tishrei).day(10)
  }

  case object Succos1 extends LoadNames("Succos") with Festival with FirstDayOf {
    override def date(year: Jewish.Year): Jewish.Day = year.month(Tishrei).day(15)
  }

  case object Succos2 extends Festival with NonFirstDayOf {
    override def names: Names = namesWithNumber(Succos1, 2)
    override def firstDay: SpecialDay = Succos1
    override def dayNumber: Int = 2
  }

  private object SuccosIntermediate extends LoadNames("Succos Intermediate")

  sealed class SuccosIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand)
  {
    final override lazy val names: Names = namesWithNumber(SuccosIntermediate, intermediateDayNumber)
    override def firstDay: SpecialDay = Succos1
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
    override def firstDay: SpecialDay = Succos1
    override def dayNumber: Int = 8
  }

  case object SimchasTorah extends LoadNames("Simchas Torah") with Festival with NonFirstDayOf {
    final override def firstDay: SpecialDay = Succos1
    override def dayNumber: Int = 9
  }

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends LoadNames("Shemini Atzeres and Simchas Torah")
    with Festival with NonFirstDayOf
  {
    final override def firstDay: SpecialDay = Succos1
    override def dayNumber: Int = 8
  }

  case object ShabbosBereishis extends LoadNames("Shabbos Bereishis") with SpecialDay {
    override def date(year: Year): Day = SimchasTorah.date(year).shabbosAfter
  }

  private object Chanukah extends LoadNames("Chanukah")

  sealed class Chanukah(override val dayNumber: Int) extends Named with DayOf with RabbinicFestival {
    final override lazy val names: Names = namesWithNumber(Chanukah, dayNumber)
    final override def firstDay: SpecialDay = Chanukah1
    final override def date(year: Year): Day = year.month(Kislev).day(25)+(dayNumber-1)
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

  sealed trait SpecialShabbos extends SpecialDay

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
    override def firstDay: SpecialDay = Pesach1
    override def dayNumber: Int = 2
  }

  private object PesachIntermediate extends LoadNames("Pesach Intermediate")

  sealed class PesachIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand)
  {
    final override lazy val names: Names = namesWithNumber(PesachIntermediate, intermediateDayNumber)
    final override def firstDay: SpecialDay = Pesach1
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
    override def firstDay: SpecialDay = Pesach1
    override def dayNumber: Int = 7
  }

  case object Pesach8 extends LoadNames("Pesach 8") with Festival with NonFirstDayOf {
    override def firstDay: SpecialDay = Pesach1
    override def dayNumber: Int = 8
  }

  case class Omer(number: Int) extends Named {
    override def names: Names = namesWithNumber(Omer, number)
  }

  object Omer extends LoadNames("Omer") {
    def dayOf(day: Day): Option[Omer] = {
      val year: Year = day.year
      val pesach: Day = Pesach1.date(year)
      val shavous: Day = Shavuos1.date(year)
      if ((day <= pesach) || (day >= shavous)) None else Some(Omer(day - pesach))
    }
  }

  case object LagBaOmer extends LoadNames("Lag Ba Omer") with SpecialDay {
    override def date(year: Year): Day = Pesach1.date(year) + 33
  }

  case object Shavuos1 extends LoadNames("Shavuos") with Festival with FirstDayOf {
    override def date(year: Year): Day = Pesach1.date(year) + 50
  }

  case object Shavuos2 extends Festival with NonFirstDayOf {
    override lazy val names: Names = namesWithNumber(Shavuos1, 2)
    override def firstDay: SpecialDay = Shavuos1
    override def dayNumber: Int = 2
  }

  case object FastOfTammuz extends LoadNames("Fast of Tammuz") with Fast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tammuz).day(17)
  }

  case object TishaBeAv extends LoadNames("Tisha BeAv") with Fast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Av).day(9)
  }

  private def namesWithNumber(withNames: Named, number: Int): Names = withNames.names.withNumber(number)

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

  val specialShabbos: Set[SpecialShabbos] =
    Set(ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh, ShabbosHagodol)

  val daysWithSpecialReadingsNotFestivals: Set[SpecialDay] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[SpecialDay] =
    festivals(inHolyLand) ++ daysWithSpecialReadingsNotFestivals

  val values: Seq[LoadNames] = Seq(
    FestivalEnd, IntermediateShabbos, RoshChodesh, ErevRoshChodesh, Fast,
    RoshHashanah1, FastOfGedalia, YomKippur, Succos1,
    SuccosIntermediate, SheminiAtzeres, SimchasTorah, SheminiAtzeresAndSimchasTorahInHolyLand,
    ShabbosBereishis, Chanukah, FastOfTeves,
    ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh, ShabbosHagodol,
    FastOfEster, Purim, ShushanPurim, Pesach1, PesachIntermediate, Pesach7, Pesach8,
    Omer, LagBaOmer, Shavuos1, FastOfTammuz, TishaBeAv
  )
}
