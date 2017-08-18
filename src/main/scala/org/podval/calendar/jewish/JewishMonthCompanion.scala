package org.podval.calendar.jewish

import org.podval.calendar.calendar.MonthCompanion

abstract class JewishMonthCompanion extends MonthCompanion[Jewish] {
  final val Name: JewishMonthName.type = JewishMonthName

  // KH 6:3
  // TODO how is this really called? tropical?
  final val meanLunarPeriod = calendar.interval.days(29).hours(12).parts(793)

  // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2):
  // BeHaRaD: (KH 6:8)
  final val firstNewMoon = calendar.moment.day(2).nightHours(5).parts(204)

  final override def yearNumber(monthNumber: Int): Int = {
    val cycleOfMonth = ((monthNumber - 1) / calendar.Year.monthsInCycle) + 1
    val yearsBeforeCycle = (cycleOfMonth - 1) * calendar.Year.yearsInCycle
    val yearMonthIsInCycle =
      calendar.Year.monthsBeforeYearInCycle.count(_ < numberInCycleOfMonth(monthNumber))
    yearsBeforeCycle + yearMonthIsInCycle
  }

  final override def numberInYear(monthNumber: Int): Int =
    numberInCycleOfMonth(monthNumber) - calendar.Year.firstMonthInCycle(yearNumber(monthNumber)) + 1

  private def numberInCycleOfMonth(monthNumber: Int): Int =
    ((monthNumber - 1) % calendar.Year.monthsInCycle) + 1
}
