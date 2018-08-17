package org.podval.calendar.jewish

import Jewish.{Year, Month, Day}
import Month.Name._

sealed trait SpecialDay {
  def apply(year: Year): Day

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
  trait Fast { this: SpecialDay =>
    final override def isFast: Boolean = true
    final override def isFestival: Boolean = false
    final override def isIntermediate: Boolean = false
    final override def isRabbinicFestival: Boolean = false
  }

  trait Festival { this: SpecialDay =>
    final override def isFast: Boolean = false
    final override def isFestival: Boolean = true
    final override def isIntermediate: Boolean = false
    final override def isRabbinicFestival: Boolean = false
  }

  trait Intermediate { this: SpecialDay =>
    final override def isFast: Boolean = false
    final override def isFestival: Boolean = false
    final override def isIntermediate: Boolean = true
    final override def isRabbinicFestival: Boolean = false
  }

  trait RabbinicFestival { this: SpecialDay =>
    final override def isFast: Boolean = false
    final override def isFestival: Boolean = true
    final override def isIntermediate: Boolean = false
    final override def isRabbinicFestival: Boolean = true
  }


  case object RoshHashanah extends SpecialDayBase(Tishrei, 1) with Festival
  case object RoshHashanah2 extends SpecialDayBase(Tishrei, 2) with Festival

  case object FastOfGedalia extends SpecialDayBase(Tishrei, 3) with Fast

  case object YomKippur extends SpecialDayBase(Tishrei, 10) with Festival

  case object Sukkot extends SpecialDayBase(Tishrei, 15) with Festival
  case object Sukkot2 extends SpecialDayBase(Tishrei, 16) with Festival
  case object SukkotIntermediate1 extends SpecialDayBase(Tishrei, 17) with Intermediate
  case object SukkotIntermediate2 extends SpecialDayBase(Tishrei, 18) with Intermediate
  case object SukkotIntermediate3 extends SpecialDayBase(Tishrei, 19) with Intermediate
  case object SukkotIntermediate4 extends SpecialDayBase(Tishrei, 20) with Intermediate
  case object SukkotIntermediate5 extends SpecialDayBase(Tishrei, 21) with Intermediate
  case object HoshanahRabbah extends SpecialDayBase(Tishrei, 21) with Intermediate
  case object ShminiAtzeret extends SpecialDayBase(Tishrei, 22) with Festival
  case object SimchatTorah extends SpecialDayBase(Tishrei, 23) with Festival

  case object SukkotIntermediate1InHolyLand extends SpecialDayBase(Tishrei, 16) with Intermediate
  case object SukkotIntermediate2InHolyLand extends SpecialDayBase(Tishrei, 17) with Intermediate
  case object SukkotIntermediate3InHolyLand extends SpecialDayBase(Tishrei, 18) with Intermediate
  case object SukkotIntermediate4InHolyLand extends SpecialDayBase(Tishrei, 19) with Intermediate
  case object SukkotIntermediate5InHolyLand extends SpecialDayBase(Tishrei, 20) with Intermediate
  case object SukkotIntermediate6InHolyLand extends SpecialDayBase(Tishrei, 21) with Intermediate
  case object ShminiAtzeretAndSimchatTorahInHolyLand extends SpecialDayBase(Tishrei, 22) with Festival

  case object ShabbosBereshit extends SpecialDay {
    override def apply(year: Year): Day = shabbosAfter(SimchatTorah(year))
    final override def isFast: Boolean = false
    final override def isFestival: Boolean = false
    final override def isIntermediate: Boolean = false
    final override def isRabbinicFestival: Boolean = false
  }

  case object Hanukkah1 extends SpecialDayBase(Kislev, 25) with RabbinicFestival
  case object Hanukkah2 extends SpecialDayBase(Kislev, 26) with RabbinicFestival
  case object Hanukkah3 extends SpecialDayBase(Kislev, 27) with RabbinicFestival
  case object Hanukkah4 extends SpecialDayBase(Kislev, 28) with RabbinicFestival
  case object Hanukkah5 extends SpecialDayBase(Kislev, 29) with RabbinicFestival
  case object Hanukkah6 extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = Hanukkah5(year)+1
  }
  case object Hanukkah7 extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = Hanukkah5(year)+2
  }
  case object Hanukkah8 extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = Hanukkah5(year)+3
  }

  case object FastOfTevet extends SpecialDayBase(Teves, 10) with Fast

  case object FastOfEster extends SpecialDay with Fast {
    override def apply(year: Year): Day = {
      val result = Purim(year)-1
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

  case object Shavuot extends SpecialDayBase(Sivan, 6) with Festival
  case object Shavuot2 extends SpecialDayBase(Sivan, 7) with Festival

  case object FastOfTammuz extends SpecialDay with Fast {
    override def apply(year: Year): Day = {
      val result = year.month(Tammuz).day(17)
      if (result.isShabbos) result+1 else result
    }
  }

  // TODO for generating Siddur, we need a list of days when Tachanun is not said;
  // also, when Av Harachamim is not said on Shabbos (by the way, when Tisha Bav is on Shabbos, it *is* said,
  // although we wouldn't have said Tachanun if it wasn't Shabbos...) - but shouldn't postponed fast
  // (or advanced Purim) leave some trace?
  case object TishaBav extends SpecialDay with Fast {
    override def apply(year: Year): Day = {
      val result = year.month(Av).day(9)
      if (result.isShabbos) result+1 else result
    }
  }


  val festivals: Set[SpecialDay] = Set(
    RoshHashanah, RoshHashanah2,
    YomKippur,
    Sukkot, Sukkot2,
    SukkotIntermediate1, SukkotIntermediate2, SukkotIntermediate3, SukkotIntermediate4, SukkotIntermediate5,
    ShminiAtzeret, SimchatTorah,
    Pesach, Pesach2,
    PesachIntermediate1, PesachIntermediate2, PesachIntermediate3, PesachIntermediate4,
    ShviiShelPesach, AcharonShelPesach,
    Shavuot, Shavuot2
  )

  val festivalsInHolyLand: Set[SpecialDay] = Set(
    RoshHashanah, RoshHashanah2,
    YomKippur,
    Sukkot,
    SukkotIntermediate1InHolyLand, SukkotIntermediate2InHolyLand, SukkotIntermediate3InHolyLand,
    SukkotIntermediate4InHolyLand, SukkotIntermediate5InHolyLand, SukkotIntermediate6InHolyLand,
    ShminiAtzeretAndSimchatTorahInHolyLand,
    Pesach, Pesach2,
    PesachIntermediate1InHolyLand, PesachIntermediate2InHolyLand, PesachIntermediate3InHolyLand,
    PesachIntermediate4InHolyLand, PesachIntermediate5InHolyLand,
    ShviiAndAcharonShelPesachInHolyLand,
    Shavuot
  )

  def festivals(inHolyLand: Boolean): Set[SpecialDay] = if (inHolyLand) festivalsInHolyLand else festivals

  val fasts: Set[SpecialDay] = Set(FastOfGedalia, FastOfTevet, FastOfEster, FastOfTammuz, TishaBav)

  val rabbinicFestivals: Set[SpecialDay] = Set(
    Hanukkah1, Hanukkah2, Hanukkah3, Hanukkah4, Hanukkah5, Hanukkah6, Hanukkah7, Hanukkah8,
    Purim // ShushanPurim
  )

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[SpecialDay] =
    festivals(inHolyLand) ++ fasts ++ rabbinicFestivals

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)
}
