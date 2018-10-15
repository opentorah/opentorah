package org.podval.calendar.jewish

import Jewish.{Day, Month, Year}
import Month.Name._
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Custom, Reading}
import org.podval.judaica.metadata.tanach.SpecialReading._

sealed trait SpecialDay {
  def apply(year: Year): Day

  def corrected(year: Year): Day = apply(year)

  // No more than one of the predicates is true

  def isFast: Boolean

  def isFestival: Boolean

  def isIntermediate: Boolean

  def isRabbinicFestival: Boolean

  def getReading(isShabbos: Boolean): Reading

  def getAfternoonReading: Option[Reading] = None
}

abstract class SpecialDayBase(month: Month.Name, numberInMonth: Int) extends SpecialDay {
  final def apply(year: Year): Day = year.month(month).day(numberInMonth)
}

// TODO 4 parshiyot: when?
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

    override def getReading(isShabbos: Boolean): Reading = {
      val aliyot1 = PublicFastTorahPart1.weekdayAliyot.get.getAliyot
      val aliyot2 = PublicFastTorahPart2.weekdayAliyot.get.getAliyot
      Reading(
        aliyot = Custom.ofCommon(aliyot1 ++ aliyot2),
        maftir = None,
        haftarah = None
      )
    }

    override def getAfternoonReading: Option[Reading] = {
      val aliyot1 = PublicFastTorahPart1.weekdayAliyot.get.getAliyot
      val aliyot2 = PublicFastTorahPart2.weekdayAliyot.get.getAliyot
      Some(Reading(
        aliyot = Custom.ofCommon(aliyot1 ++ aliyot2.init),
        maftir = Some(aliyot2.last),
        haftarah = Some(PublicFastTorahPart2.haftarah.get)
      ))
    }
  }

  case object RoshHashanah extends SpecialDayBase(Tishrei, 1) with Festival {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = RoshHashanah1Reading,
      maftir = RoshHashanah1Reading,
      haftarah = RoshHashanah1Reading
    )
  }

  case object RoshHashanah2 extends SpecialDayBase(Tishrei, 2) with Festival {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = RoshHashanah2Reading,
      maftir = RoshHashanah1Reading,
      haftarah = RoshHashanah2Reading
    )
  }

  case object FastOfGedalia extends SpecialDayBase(Tishrei, 3) with Fast {
    override def corrected(year: Year): Day = {
      val result = apply(year)
      if (result.isShabbos) result+1 else result
    }
  }

  case object YomKippur extends SpecialDayBase(Tishrei, 10) with Festival {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = YomKippurShacharis,
      maftir = YomKippurShacharis,
      haftarah = YomKippurShacharis
    )

    override def getAfternoonReading: Option[Reading] = {
      val aliyot: Seq[ChumashSpan.BookSpan] = YomKippurMincha.weekdayAliyot.get.getAliyot
      Some(Reading(
        aliyot = Custom.ofCommon(aliyot.init),
        maftir = Some(aliyot.last),
        haftarah = Some(YomKippurMincha.haftarah.get)
      ))
    }
  }

  case object Succos extends SpecialDayBase(Tishrei, 15) with Festival {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = Succos1_2.getAliyot(isShabbos).get,
      maftir = Succos1_2,
      haftarah = Succos1_2
    )
  }

  case object Succos2 extends SpecialDayBase(Tishrei, 16) with Festival {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = Succos1_2.getAliyot(isShabbos).get,
      maftir = Succos1_2,
      haftarah = Succos2Haftarah
    )
  }

  // TODO mapping to the readings skips Shabbos - whatever that means...
  class SuccosIntermediate(month: Month.Name, numberInMonth: Int, val number: Int)
    extends SpecialDayBase(month, numberInMonth) with Intermediate
  {
    override def getReading(isShabbos: Boolean): Reading = ???
  }

  case object SuccosIntermediate1 extends SuccosIntermediate(Tishrei, 17, 1)
  case object SuccosIntermediate2 extends SuccosIntermediate(Tishrei, 18, 2)
  case object SuccosIntermediate3 extends SuccosIntermediate(Tishrei, 19, 3)
  case object SuccosIntermediate4 extends SuccosIntermediate(Tishrei, 20, 4)
  case object HoshanahRabbah extends SuccosIntermediate(Tishrei, 21, 5)

  case object ShminiAtzeret extends SpecialDayBase(Tishrei, 22) with Festival {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = ShminiAtzeresReading.getAliyot(isShabbos).get,
      maftir = ShminiAtzeresReading,
      haftarah = ShminiAtzeresReading
    )
  }

  case object SimchatTorah extends SpecialDayBase(Tishrei, 23) with Festival {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = SimchasTorahReading.weekdayAliyot.get.getAliyot ++ SimchasTorahChassanBereishis.weekdayAliyot.get.getAliyot,
      maftir = ShminiAtzeresReading,
      haftarah = SimchasTorahReading
    )
  }

  case object SuccosIntermediate1InHolyLand extends SuccosIntermediate(Tishrei, 16, 1)
  case object SuccosIntermediate2InHolyLand extends SuccosIntermediate(Tishrei, 17, 2)
  case object SuccosIntermediate3InHolyLand extends SuccosIntermediate(Tishrei, 18, 3)
  case object SuccosIntermediate4InHolyLand extends SuccosIntermediate(Tishrei, 19, 4)
  case object SuccosIntermediate5InHolyLand extends SuccosIntermediate(Tishrei, 20, 5)
  case object HoshanahRabbahInHolyLand extends SuccosIntermediate(Tishrei, 21, 6)

  case object ShminiAtzeretAndSimchatTorahInHolyLand extends SpecialDayBase(Tishrei, 22) with Festival {
    def getReading(isShabbos: Boolean): Reading = SimchatTorah.getReading(isShabbos)
  }

  // TODO Shabbos Chanukkah *adds* to the weeklyReading!
  sealed class Chanukkah(val number: Int) extends RabbinicFestival {
    override def apply(year: Year): Day = Chanukah1(year)+(number-1)

    override def getReading(isShabbos: Boolean): Reading = ???
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

  case object Purim extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = year.month(if (year.isLeap) AdarII else Adar).day(14)

    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = PurimReading,
      maftir = PurimReading,
      haftarah = PurimReading
    )
  }

  // TODO if there is no reading of the Torah - remove.
  case object ShushanPurim extends SpecialDay with RabbinicFestival {
    override def apply(year: Year): Day = Purim(year)+1

    override def getReading(isShabbos: Boolean): Reading = ???
  }

  abstract class PesachDay(month: Month.Name, numberInMonth: Int, val number: Int) extends SpecialDayBase(month, numberInMonth)
  {
    override def getReading(isShabbos: Boolean): Reading = ???
  }

  case object Pesach extends PesachDay(Nisan, 15, 1) with Festival
  case object Pesach2 extends PesachDay(Nisan, 16, 2) with Festival

  // TODO mapping to the readings skips Shabbos!
  case object PesachIntermediate1 extends PesachDay(Nisan, 17, 3) with Intermediate
  case object PesachIntermediate2 extends PesachDay(Nisan, 18, 4) with Intermediate
  case object PesachIntermediate3 extends PesachDay(Nisan, 19, 5) with Intermediate
  case object PesachIntermediate4 extends PesachDay(Nisan, 20, 6) with Intermediate
  case object ShviiShelPesach extends PesachDay(Nisan, 21, 7) with Festival
  case object AcharonShelPesach extends PesachDay(Nisan, 22, 8) with Festival

  case object PesachIntermediate1InHolyLand extends PesachDay(Nisan, 16, 2) with Intermediate
  case object PesachIntermediate2InHolyLand extends PesachDay(Nisan, 17, 3) with Intermediate
  case object PesachIntermediate3InHolyLand extends PesachDay(Nisan, 18, 4) with Intermediate
  case object PesachIntermediate4InHolyLand extends PesachDay(Nisan, 19, 5) with Intermediate
  case object PesachIntermediate5InHolyLand extends PesachDay(Nisan, 20, 6) with Intermediate
  case object ShviiAndAcharonShelPesachInHolyLand extends PesachDay(Nisan, 21, 7) with Festival

  case object Shavuos extends SpecialDayBase(Sivan, 6) with Festival {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = Shavuos1Reading,
      maftir = Shavuos1Reading,
      haftarah = Shavuos1Reading
    )
  }

  case object Shavuos2 extends SpecialDayBase(Sivan, 7) with Festival {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = (if (isShabbos) ShminiAtzeresReading.shabbosAliyot else Pesach8Reading.weekdayAliyot).get,
      maftir = Shavuos1Reading,
      haftarah = Shavuos2Haftarah
    )
  }

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

    override def getReading(isShabbos: Boolean): Reading = {
      val aliyot = TishaBeAvReading.weekdayAliyot.get.getAliyot
      Reading(
        aliyot = Custom.ofCommon(aliyot.init),
        maftir = Some(aliyot.last),
        haftarah = Some(TishaBeAvReading.haftarah.get)
      )
    }

    override def getAfternoonReading: Option[Reading] = Some(getReading(isShabbos = false))
  }


  val festivals: Set[FestivalOrIntermediate] = Set(
    RoshHashanah, RoshHashanah2,
    YomKippur,
    Succos, Succos2,
    SuccosIntermediate1, SuccosIntermediate2, SuccosIntermediate3, SuccosIntermediate4,
    HoshanahRabbah, ShminiAtzeret, SimchatTorah,
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
    SuccosIntermediate4InHolyLand, SuccosIntermediate5InHolyLand,
    HoshanahRabbahInHolyLand, ShminiAtzeretAndSimchatTorahInHolyLand,
    Pesach,
    PesachIntermediate1InHolyLand, PesachIntermediate2InHolyLand, PesachIntermediate3InHolyLand,
    PesachIntermediate4InHolyLand, PesachIntermediate5InHolyLand,
    ShviiAndAcharonShelPesachInHolyLand,
    Shavuos
  )

  def festivals(inHolyLand: Boolean): Set[FestivalOrIntermediate] = if (inHolyLand) festivalsInHolyLand else festivals

  val rabbinicFestivals: Set[RabbinicFestival] = Set(
    Chanukah1, Chanukah2, Chanukah3, Chanukah4, Chanukah5, Chanukah6, Chanukah7, Chanukah8,
    Purim // ShushanPurim
  )

  // TODO use corrected() istead of apply() for Torah readings; double-check Purim.
  val fasts: Set[Fast] = Set(FastOfGedalia, FastOfTeves, FastOfEster, FastOfTammuz, TishaBeAv)

  val daysWithSpecialReadingsNotFestivals: Set[SpecialDay] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[SpecialDay] =
    festivals(inHolyLand) ++ fasts ++ rabbinicFestivals

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)

  def shabbosBereishis(year: Year): Day = shabbosAfter(SimchatTorah(year))

  def lagBaOmer(year: Year): Day = year.month(Iyar).day(18)
}
