package org.podval.calendar.jewish

import Jewish.{Year, Month, Day}
import Month.Name._

sealed trait SpecialDay {
  def apply(year: Year): Day

  def corrected(year: Year): Day = apply(year)

  // No more than one of the predicates is true

  def isFast: Boolean

  def isFestival: Boolean

  def isIntermediate: Boolean

  def isRabbinicFestival: Boolean
}

abstract class SpecialDayBase(month: Month.Name, numberInMonth: Int) extends SpecialDay {
  final def apply(year: Year): Day = year.month(month).day(numberInMonth)
}

object SpecialDay {
  trait FestivalOrIntermediate extends SpecialDay

  trait Festival extends FestivalOrIntermediate {
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

  trait Fast extends SpecialDay {
    final override def isFast: Boolean = true
    final override def isFestival: Boolean = false
    final override def isIntermediate: Boolean = false
    final override def isRabbinicFestival: Boolean = false
  }

  case object RoshHashanah extends SpecialDayBase(Tishrei, 1) with Festival
  case object RoshHashanah2 extends SpecialDayBase(Tishrei, 2) with Festival

  case object FastOfGedalia extends SpecialDayBase(Tishrei, 3) with Fast {
    override def corrected(year: Year): Day = {
      val result = apply(year)
      if (result.isShabbos) result+1 else result
    }
  }

  case object YomKippur extends SpecialDayBase(Tishrei, 10) with Festival

  case object Succos extends SpecialDayBase(Tishrei, 15) with Festival
  case object Succos2 extends SpecialDayBase(Tishrei, 16) with Festival
  // TODO mapping to the readings skips Shabbos!
  case object SuccosIntermediate1 extends SpecialDayBase(Tishrei, 17) with Intermediate
  case object SuccosIntermediate2 extends SpecialDayBase(Tishrei, 18) with Intermediate
  case object SuccosIntermediate3 extends SpecialDayBase(Tishrei, 19) with Intermediate
  case object SuccosIntermediate4 extends SpecialDayBase(Tishrei, 20) with Intermediate
  case object SuccosIntermediate5 extends SpecialDayBase(Tishrei, 21) with Intermediate
  case object HoshanahRabbah extends SpecialDayBase(Tishrei, 21) with Intermediate
  case object ShminiAtzeret extends SpecialDayBase(Tishrei, 22) with Festival
  case object SimchatTorah extends SpecialDayBase(Tishrei, 23) with Festival

  case object SuccosIntermediate1InHolyLand extends SpecialDayBase(Tishrei, 16) with Intermediate
  case object SuccosIntermediate2InHolyLand extends SpecialDayBase(Tishrei, 17) with Intermediate
  case object SuccosIntermediate3InHolyLand extends SpecialDayBase(Tishrei, 18) with Intermediate
  case object SuccosIntermediate4InHolyLand extends SpecialDayBase(Tishrei, 19) with Intermediate
  case object SuccosIntermediate5InHolyLand extends SpecialDayBase(Tishrei, 20) with Intermediate
  case object ShminiAtzeretAndSimchatTorahInHolyLand extends SpecialDayBase(Tishrei, 22) with Festival

  case object Chanukah1 extends SpecialDayBase(Kislev, 25) with RabbinicFestival
  case object Chanukah2 extends SpecialDayBase(Kislev, 26) with RabbinicFestival
  case object Chanukah3 extends SpecialDayBase(Kislev, 27) with RabbinicFestival
  case object Chanukah4 extends SpecialDayBase(Kislev, 28) with RabbinicFestival
  case object Chanukah5 extends SpecialDayBase(Kislev, 29) with RabbinicFestival
  case object Chanukah6 extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = Chanukah5(year)+1
  }
  case object Chanukah7 extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = Chanukah5(year)+2
  }
  case object Chanukah8 extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = Chanukah5(year)+3
  }

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

  case object Purim extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = year.month(if (year.isLeap) AdarII else Adar).day(14)
  }

  case object ShushanPurim extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = Purim(year)+1
  }

  case object Pesach extends SpecialDayBase(Nisan, 15) with Festival
  case object Pesach2 extends SpecialDayBase(Nisan, 16) with Festival

  // TODO mapping to the readings skips Shabbos!
  case object PesachIntermediate1 extends SpecialDayBase(Nisan, 17) with Intermediate
  case object PesachIntermediate2 extends SpecialDayBase(Nisan, 18) with Intermediate
  case object PesachIntermediate3 extends SpecialDayBase(Nisan, 19) with Intermediate
  case object PesachIntermediate4 extends SpecialDayBase(Nisan, 20) with Intermediate
  case object ShviiShelPesach extends SpecialDayBase(Nisan, 21) with Festival
  case object AcharonShelPesach extends SpecialDayBase(Nisan, 22) with Festival

  case object PesachIntermediate1InHolyLand extends SpecialDayBase(Nisan, 16) with Intermediate
  case object PesachIntermediate2InHolyLand extends SpecialDayBase(Nisan, 17) with Intermediate
  case object PesachIntermediate3InHolyLand extends SpecialDayBase(Nisan, 18) with Intermediate
  case object PesachIntermediate4InHolyLand extends SpecialDayBase(Nisan, 19) with Intermediate
  case object PesachIntermediate5InHolyLand extends SpecialDayBase(Nisan, 20) with Intermediate
  case object ShviiAndAcharonShelPesachInHolyLand extends SpecialDayBase(Nisan, 21) with Festival

  case object LagBaOmer extends SpecialDayBase(Iyar, 18) {
    final override def isFast: Boolean = false
    final override def isFestival: Boolean = false
    final override def isIntermediate: Boolean = false
    final override def isRabbinicFestival: Boolean = false
  }

  case object Shavuos extends SpecialDayBase(Sivan, 6) with Festival
  case object Shavuos2 extends SpecialDayBase(Sivan, 7) with Festival

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
  }


  val festivals: Set[FestivalOrIntermediate] = Set(
    RoshHashanah, RoshHashanah2,
    YomKippur,
    Succos, Succos2,
    SuccosIntermediate1, SuccosIntermediate2, SuccosIntermediate3, SuccosIntermediate4, SuccosIntermediate5,
    ShminiAtzeret, SimchatTorah,
    Pesach, Pesach2,
    PesachIntermediate1, PesachIntermediate2, PesachIntermediate3, PesachIntermediate4,
    ShviiShelPesach, AcharonShelPesach,
    Shavuos, Shavuos2
  )

  val festivalsInHolyLand: Set[FestivalOrIntermediate] = Set(
    RoshHashanah, RoshHashanah2,
    YomKippur,
    Succos,
    SuccosIntermediate1InHolyLand, SuccosIntermediate2InHolyLand, SuccosIntermediate3InHolyLand,
    SuccosIntermediate4InHolyLand, SuccosIntermediate5InHolyLand, HoshanahRabbah,
    ShminiAtzeretAndSimchatTorahInHolyLand,
    Pesach, Pesach2,
    PesachIntermediate1InHolyLand, PesachIntermediate2InHolyLand, PesachIntermediate3InHolyLand,
    PesachIntermediate4InHolyLand, PesachIntermediate5InHolyLand,
    ShviiAndAcharonShelPesachInHolyLand,
    Shavuos
  )

  def festivals(inHolyLand: Boolean): Set[FestivalOrIntermediate] = if (inHolyLand) festivalsInHolyLand else festivals

  // TODO use corrected() istead of apply() for Torah readings; double-check Purim.
  val fasts: Set[Fast] = Set(FastOfGedalia, FastOfTeves, FastOfEster, FastOfTammuz, TishaBeAv)

  val rabbinicFestivals: Set[RabbinicFestival] = Set(
    Chanukah1, Chanukah2, Chanukah3, Chanukah4, Chanukah5, Chanukah6, Chanukah7, Chanukah8,
    Purim // ShushanPurim
  )

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[SpecialDay] =
    festivals(inHolyLand) ++ fasts ++ rabbinicFestivals

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)

  def shabbosBereishis(year: Year): Day = shabbosAfter(SimchatTorah(year))
}
