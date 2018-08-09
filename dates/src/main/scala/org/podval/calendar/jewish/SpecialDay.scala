package org.podval.calendar.jewish

import Jewish.{Year, Month, Day}

class SpecialDay(month: Month.Name, numberInMonth: Int, length: Int = 1, shorterInHolyLand: Boolean = false) {
  def start(year: Year): Day =
    year.month(month).day(numberInMonth)

  def days(year: Year, inHolyLand: Boolean): Seq[Day] =
    for (numDay <- 0 until (if (shorterInHolyLand && inHolyLand) length-1 else length))
    yield start(year) + numDay
}

// TODO length in days
// TODO festival/cholhamoed/fast/etc.
object SpecialDay {

  case object RoshHashanah extends SpecialDay(Month.Name.Tishrei, 1, 2)

  case object YomKippur extends SpecialDay(Month.Name.Tishrei, 10)

  case object Sukkot extends SpecialDay(Month.Name.Tishrei, 15, length = 9, shorterInHolyLand = true)

//  // SimchatTorah in the Holy Land
//  case object ShminiAtzeret extends SpecialDay(Month.Name.Tishrei, 22)
//  // SimchatTorah outside of the Holy Land
//  case object ShminiAtzeret2 extends SpecialDay(Month.Name.Tishrei, 23)

  case object Hanukkah extends SpecialDay(Month.Name.Kislev, 25, 8)

//  // TODO Shushan Purim; leap year...
//  case object Purim extends SpecialDay(Month.Name.Adar, 14)

  case object Pesach extends SpecialDay(Month.Name.Nisan, 15, 8, true)

  case object Shavuot extends SpecialDay(Month.Name.Sivan, 6, 2, true)

  case object TishaBAv extends SpecialDay(Month.Name.Av, 9)
}
