package org.podval.calendar.jewish

import Jewish.{Day, Month, Year}
import Month.Name._
import org.podval.judaica.metadata.tanach.{Reading, WeeklyReading}
import org.podval.judaica.metadata.tanach.SpecialReading

sealed trait SpecialDay {
  def apply(year: Year): Day

  def corrected(year: Year): Day = apply(year)
}

// TODO add metadata/names!
object SpecialDay {

  abstract class SpecialDayBase(month: Month.Name, numberInMonth: Int) extends SpecialDay {
    final def apply(year: Year): Day = year.month(month).day(numberInMonth)
  }

  trait WithReading {
    def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading

    def getAfternoonReading: Option[Reading] = None
  }

  trait SpecialDayWithReading extends SpecialDay with WithReading

  trait WithSimpleReading extends WithReading {
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

  abstract class SpecialDayWithSimpleReading(month: Month.Name, numberInMonth: Int, reading: SpecialReading)
    extends SpecialDayBase(month, numberInMonth) with WithSimpleReading
  {
    protected final override def getReading(isShabbos: Boolean): Reading = reading.getReading(isShabbos)

    final override def getAfternoonReading: Option[Reading] = reading.getAfternoonReading
  }

  trait FestivalOrIntermediate extends SpecialDayWithReading

  trait Festival extends FestivalOrIntermediate with WithSimpleReading

  trait Intermediate extends FestivalOrIntermediate

  trait RabbinicFestival extends SpecialDayWithReading

  trait Fast extends SpecialDayWithReading with WithSimpleReading {
    protected override def getReading(isShabbos: Boolean): Reading =
      SpecialReading.Fast.getReading(isShabbos)
  }

  case object RoshHashanah1 extends SpecialDayWithSimpleReading(Tishrei, 1, SpecialReading.RoshHashanah1) with Festival
  case object RoshHashanah2 extends SpecialDayWithSimpleReading(Tishrei, 2, SpecialReading.RoshHashanah2) with Festival

  case object FastOfGedalia extends SpecialDayBase(Tishrei, 3) with Fast {
    override def corrected(year: Year): Day = {
      val result = apply(year)
      if (result.isShabbos) result+1 else result
    }

    override def getReading(isShabbos: Boolean): Reading =
      SpecialReading.FastOfGedalia.getReading(isShabbos)
  }

  case object YomKippur extends SpecialDayWithSimpleReading(Tishrei, 10, SpecialReading.YomKippur) with Festival

  case object Succos extends SpecialDayWithSimpleReading(Tishrei, 15, SpecialReading.Succos) with Festival
  case object Succos2 extends SpecialDayWithSimpleReading(Tishrei, 16, SpecialReading.Succos2) with Festival

  // TODO mapping to the readings skips Shabbos - whatever that means... Or was it meant for Pesach only?
  class SuccosIntermediate(month: Month.Name, numberInMonth: Int, number: Int, inHolyLand: Boolean)
    extends SpecialDayBase(month, numberInMonth) with Intermediate with WithSimpleReading
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

  case object SheminiAtzeret extends SpecialDayWithSimpleReading(Tishrei, 22, SpecialReading.SheminiAtzeres) with Festival
  case object SimchasTorah extends SpecialDayWithSimpleReading(Tishrei, 23, SpecialReading.SimchasTorah) with Festival

  case object ShminiAtzeretAndSimchatTorahInHolyLand extends
    SpecialDayWithSimpleReading(Tishrei, 22, SpecialReading.SimchasTorah) with Festival

  case object ShabbosBereishis extends SpecialDay {
    override def apply(year: Year): Day = shabbosAfter(SimchasTorah(year))
  }

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

  case object ParshasShekalim extends SpecialDay {
    override def apply(year: Year): Day = {
      val result = Purim(year).month.firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }
  }

  case object ParshasZachor extends SpecialDay {
    override def apply(year: Year): Day = shabbosBefore(Purim(year))
  }

  case object ParshasParah extends SpecialDay {
    override def apply(year: Year): Day = shabbosBefore(ParshasHachodesh(year))
  }

  case object ParshasHachodesh extends SpecialDay {
    override def apply(year: Year): Day = {
      val result = year.month(Nisan).firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }
  }

  case object ShabbosHagodol extends SpecialDay {
    override def apply(year: Year): Day = shabbosBefore(Pesach(year))
  }

  case object Purim extends SpecialDay with RabbinicFestival with WithSimpleReading {
    override def apply(year: Year): Day = year.month(if (year.isLeap) AdarII else Adar).day(14)

    protected override def getReading(isShabbos: Boolean): Reading =
      SpecialReading.Purim.getReading(isShabbos)
  }

  case object ShushanPurim extends SpecialDay {
    override def apply(year: Year): Day = Purim(year) + 1
  }

  case object Pesach extends SpecialDayWithSimpleReading(Nisan, 15, SpecialReading.Pesach) with Festival
  case object Pesach2 extends SpecialDayWithSimpleReading(Nisan, 16, SpecialReading.Pesach2) with Festival
  case object Pesach2InHolyLand extends SpecialDayWithSimpleReading(Nisan, 16, SpecialReading.Pesach2InHolyLand) with Intermediate
  case object Pesach3 extends SpecialDayWithSimpleReading(Nisan, 17, SpecialReading.Pesach3) with Intermediate

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

  case object Pesach6 extends SpecialDayWithSimpleReading(Nisan, 20, SpecialReading.Pesach6) with Intermediate
  case object Pesach7 extends SpecialDayWithSimpleReading(Nisan, 21, SpecialReading.Pesach7) with Festival
  case object Pesach8 extends SpecialDayWithSimpleReading(Nisan, 22, SpecialReading.Pesach8) with Festival

  case object LagBaOmer extends SpecialDayBase(Iyar, 18)

  case object Shavuos extends SpecialDayWithSimpleReading(Sivan, 6, SpecialReading.Shavuos) with Festival
  case object Shavuos2 extends SpecialDayWithSimpleReading(Sivan, 7, SpecialReading.Shavuos2) with Festival

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

  val daysWithSpecialReadingsNotFestivals: Set[SpecialDayWithReading] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[SpecialDayWithReading] =
    Set.empty[SpecialDayWithReading] ++ festivals(inHolyLand) ++ fasts ++ rabbinicFestivals

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)
}
