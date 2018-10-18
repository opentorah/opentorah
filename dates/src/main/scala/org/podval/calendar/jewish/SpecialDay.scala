package org.podval.calendar.jewish

import Jewish.{Day, Month, Year}
import Month.Name._
import org.podval.judaica.metadata.tanach.{Reading, WeeklyReading}
import org.podval.judaica.metadata.tanach.SpecialReading

sealed trait SpecialDay {
  def apply(year: Year): Day

  def corrected(year: Year): Day = apply(year)

  // No more than one of the predicates is true

  def isFast: Boolean

  def isFestival: Boolean

  def isIntermediate: Boolean

  def isRabbinicFestival: Boolean

  def getReading(
    isShabbos: Boolean,
    isRoshChodesh: Boolean,
    weeklyReading: Option[WeeklyReading],
    isPesachOnChamishi: Boolean
  ): Reading

  def getAfternoonReading: Option[Reading] = None
}

// TODO 4 parshiyot: when?
object SpecialDay {

  abstract class SpecialDayBase(month: Month.Name, numberInMonth: Int) extends SpecialDay {
    final def apply(year: Year): Day = year.month(month).day(numberInMonth)
  }

  trait SimpleGetReading extends SpecialDay {
    final override def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading = {
      require(weeklyReading.isEmpty)
      getReading(isShabbos)
    }

    protected def getReading(isShabbos: Boolean): Reading
  }

  abstract class SimpleSpecialDay(month: Month.Name, numberInMonth: Int, reading: SpecialReading)
    extends SpecialDayBase(month, numberInMonth) with SimpleGetReading
  {
    protected final override def getReading(isShabbos: Boolean): Reading = reading.getReading(isShabbos)
  }

  trait FestivalOrIntermediate extends SpecialDay

  trait Festival extends FestivalOrIntermediate  with SimpleGetReading {
    final override def isFast: Boolean = false
    final override def isFestival: Boolean = true
    final override def isIntermediate: Boolean = false
    final override def isRabbinicFestival: Boolean = false
  }

  trait Intermediate extends FestivalOrIntermediate {
    final override def isFast: Boolean = false
    final override def isFestival: Boolean = false
    final override def isIntermediate: Boolean = true
    final override def isRabbinicFestival: Boolean = false
  }

  trait RabbinicFestival extends SpecialDay {
    final override def isFast: Boolean = false
    final override def isFestival: Boolean = true
    final override def isIntermediate: Boolean = false
    final override def isRabbinicFestival: Boolean = true
  }

  trait Fast extends SpecialDay with SimpleGetReading {
    final override def isFast: Boolean = true
    final override def isFestival: Boolean = false
    final override def isIntermediate: Boolean = false
    final override def isRabbinicFestival: Boolean = false

    protected override def getReading(isShabbos: Boolean): Reading =
      SpecialReading.Fast.getReading(isShabbos)

    override def getAfternoonReading: Option[Reading] =
      Some(SpecialReading.Fast.getAfternoonReading)
  }

  case object RoshHashanah1 extends SimpleSpecialDay(Tishrei, 1, SpecialReading.RoshHashanah1) with Festival
  case object RoshHashanah2 extends SimpleSpecialDay(Tishrei, 2, SpecialReading.RoshHashanah2) with Festival

  case object FastOfGedalia extends SpecialDayBase(Tishrei, 3) with Fast {
    override def corrected(year: Year): Day = {
      val result = apply(year)
      if (result.isShabbos) result+1 else result
    }
  }

  case object YomKippur extends SimpleSpecialDay(Tishrei, 10, SpecialReading.YomKippur) with Festival {
    override def getAfternoonReading: Option[Reading] =
      Some(SpecialReading.YomKippurAfternoon.getReading(false))
  }

  case object Succos extends SimpleSpecialDay(Tishrei, 15, SpecialReading.Succos) with Festival
  case object Succos2 extends SimpleSpecialDay(Tishrei, 16, SpecialReading.Succos2) with Festival

  // TODO mapping to the readings skips Shabbos - whatever that means... Or was it meant for Pesach only?
  class SuccosIntermediate(month: Month.Name, numberInMonth: Int, number: Int, inHolyLand: Boolean)
    extends SpecialDayBase(month, numberInMonth) with Intermediate with SimpleGetReading
  {
    protected override def getReading(isShabbos: Boolean): Reading =
      SpecialReading.SuccosIntermediate.getReading(isShabbos, number, inHolyLand)
  }

  case object SuccosIntermediate1 extends SuccosIntermediate(Tishrei, 17, 1, false)
  case object SuccosIntermediate2 extends SuccosIntermediate(Tishrei, 18, 2, false)
  case object SuccosIntermediate3 extends SuccosIntermediate(Tishrei, 19, 3, false)
  case object SuccosIntermediate4 extends SuccosIntermediate(Tishrei, 20, 4, false)
  case object HoshanahRabbah extends SuccosIntermediate(Tishrei, 21, 5, false)

  case object SuccosIntermediate1InHolyLand extends SuccosIntermediate(Tishrei, 16, 1, true)
  case object SuccosIntermediate2InHolyLand extends SuccosIntermediate(Tishrei, 17, 2, true)
  case object SuccosIntermediate3InHolyLand extends SuccosIntermediate(Tishrei, 18, 3, true)
  case object SuccosIntermediate4InHolyLand extends SuccosIntermediate(Tishrei, 19, 4, true)
  case object SuccosIntermediate5InHolyLand extends SuccosIntermediate(Tishrei, 20, 5, true)
  case object HoshanahRabbahInHolyLand extends SuccosIntermediate(Tishrei, 21, 6, true)

  case object SheminiAtzeret extends SimpleSpecialDay(Tishrei, 22, SpecialReading.SheminiAtzeres) with Festival
  case object SimchasTorah extends SimpleSpecialDay(Tishrei, 23, SpecialReading.SimchasTorah) with Festival

  case object ShminiAtzeretAndSimchatTorahInHolyLand extends
    SimpleSpecialDay(Tishrei, 22, SpecialReading.SimchasTorah) with Festival

  sealed class Chanukkah(number: Int) extends RabbinicFestival {
    override def apply(year: Year): Day = Chanukah1(year)+(number-1)

    override def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading =
      SpecialReading.Channukah.getReading(
        isShabbos = isShabbos,
        number = number,
        isRoshChodesh = isRoshChodesh,
        weeklyReading = weeklyReading
      )
  }
  case object Chanukah1 extends Chanukkah(1) {
    override def apply(year: Year): Day = year.month(Kislev).day(25)
  }
  case object Chanukah2 extends Chanukkah(2)
  case object Chanukah3 extends Chanukkah(3)
  case object Chanukah4 extends Chanukkah(4)
  case object Chanukah5 extends Chanukkah(5)
  case object Chanukah6 extends Chanukkah(6)
  case object Chanukah7 extends Chanukkah(7)
  case object Chanukah8 extends Chanukkah(8)

  case object FastOfTeves extends SpecialDayBase(Teves, 10) with Fast

  case object FastOfEster extends SpecialDay with Fast {
    override def apply(year: Year): Day = Purim(year)-1

    override def corrected(year: Year): Day = {
      val result = apply(year)
      // If on Friday or Saturday - move to Thursday
      if (result.isShabbos) result-2 else
      if (result.next.isShabbos) result-1 else
        result
    }
  }

  case object Purim extends SpecialDay with RabbinicFestival with SimpleGetReading {
    override def apply(year: Year): Day = year.month(if (year.isLeap) AdarII else Adar).day(14)

    protected override def getReading(isShabbos: Boolean): Reading =
      SpecialReading.Purim.getReading(isShabbos)
  }

  case object Pesach extends SimpleSpecialDay(Nisan, 15, SpecialReading.Pesach) with Festival
  case object Pesach2 extends SimpleSpecialDay(Nisan, 16, SpecialReading.Pesach2) with Festival
  case object Pesach2InHolyLand extends SimpleSpecialDay(Nisan, 16, SpecialReading.Pesach2InHolyLand) with Intermediate
  case object Pesach3 extends SimpleSpecialDay(Nisan, 17, SpecialReading.Pesach3) with Intermediate

  case object Pesach4 extends SpecialDayBase(Nisan, 18) with Intermediate {
    override def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading = SpecialReading.Pesach4.getReading(isShabbos, isPesachOnChamishi)
  }

  case object Pesach5 extends SpecialDayBase(Nisan, 19) with Intermediate {
    override def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading = SpecialReading.Pesach5.getReading(isShabbos, isPesachOnChamishi)
  }

  case object Pesach6 extends SimpleSpecialDay(Nisan, 20, SpecialReading.Pesach6) with Intermediate
  case object Pesach7 extends SimpleSpecialDay(Nisan, 21, SpecialReading.Pesach7) with Festival
  case object Pesach8 extends SimpleSpecialDay(Nisan, 22, SpecialReading.Pesach8) with Festival

  case object Shavuos extends SimpleSpecialDay(Sivan, 6, SpecialReading.Shavuos) with Festival
  case object Shavuos2 extends SimpleSpecialDay(Sivan, 7, SpecialReading.Shavuos2) with Festival

  case object FastOfTammuz extends SpecialDayBase(Tammuz, 17) with Fast {
    override def corrected(year: Year): Day = {
      val result = apply(year)
      if (result.isShabbos) result+1 else result
    }
  }

  case object TishaBeAv extends SpecialDayBase(Av, 9) with Fast {
    override def corrected(year: Year): Day = {
      val result = apply(year)
      if (result.isShabbos) result+1 else result
    }

    override def getReading(isShabbos: Boolean): Reading =
      SpecialReading.TishaBeAv.getReading(isShabbos)

    override def getAfternoonReading: Option[Reading] =
      Some(getReading(isShabbos = false))
  }


  val festivals: Set[FestivalOrIntermediate] = Set(
    RoshHashanah1, RoshHashanah2,
    YomKippur,
    Succos, Succos2,
    SuccosIntermediate1, SuccosIntermediate2, SuccosIntermediate3, SuccosIntermediate4,
    HoshanahRabbah, SheminiAtzeret, SimchasTorah,
    Pesach, Pesach2, Pesach3, Pesach4, Pesach5, Pesach6, Pesach7, Pesach8,
    Shavuos, Shavuos2
  )

  val festivalsInHolyLand: Set[FestivalOrIntermediate] = Set(
    RoshHashanah1, RoshHashanah2,
    YomKippur,
    Succos,
    SuccosIntermediate1InHolyLand, SuccosIntermediate2InHolyLand, SuccosIntermediate3InHolyLand,
    SuccosIntermediate4InHolyLand, SuccosIntermediate5InHolyLand,
    HoshanahRabbahInHolyLand, ShminiAtzeretAndSimchatTorahInHolyLand,
    Pesach, Pesach2InHolyLand, Pesach3, Pesach4, Pesach5, Pesach6, Pesach7,
    Shavuos
  )

  def festivals(inHolyLand: Boolean): Set[FestivalOrIntermediate] = if (inHolyLand) festivalsInHolyLand else festivals

  val rabbinicFestivals: Set[RabbinicFestival] = Set(
    Chanukah1, Chanukah2, Chanukah3, Chanukah4, Chanukah5, Chanukah6, Chanukah7, Chanukah8,
    Purim // ShushanPurim
  )

  val fasts: Set[Fast] = Set(FastOfGedalia, FastOfTeves, FastOfEster, FastOfTammuz, TishaBeAv)

  val daysWithSpecialReadingsNotFestivals: Set[SpecialDay] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[SpecialDay] =
    festivals(inHolyLand) ++ fasts ++ rabbinicFestivals

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)

  def shabbosBereishis(year: Year): Day = shabbosAfter(SimchasTorah(year))

  def shushanPurim(year: Year): Day = Purim(year)+1

  def lagBaOmer(year: Year): Day = year.month(Iyar).day(18)
}
