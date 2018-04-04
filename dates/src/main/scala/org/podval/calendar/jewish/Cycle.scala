package org.podval.calendar.jewish

import Jewish.TimeInterval

object Cycle {
  final val yearsInCycle: Int = 19

  final val leapYears: Set[Int] =
    Set(3, 6, 8, 11, 14, 17, 19)

  final val leapYearsInCycle: Int = leapYears.size

  final def isLeapYear(yearNumber: Int): Boolean =
    leapYears.contains(Cycle.yearNumberInCycle(yearNumber))

  final def yearCycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1

  final def yearNumberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1

  final def yearInCycle(yearCycle: Int, yearNumberInCycle: Int): Int =
    yearsInCycle * (yearCycle-1) + yearNumberInCycle

  final def firstYearInCycle(cycleNumber: Int): Int = yearInCycle(cycleNumber, 0)

  final def yearLengthInMonths(yearNumber: Int): Int = yearLengthInMonths(isLeapYear(yearNumber))

  final def yearLengthInMonths(isLeap: Boolean): Int = if (isLeap) 13 else 12

  final val monthsBeforeYearInCycle: Seq[Int] =
    ((1 to yearsInCycle) map yearLengthInMonths).scanLeft(0)(_ + _)

  final def firstMonthInCycle(yearNumber: Int): Int =
    monthsBeforeYearInCycle(yearNumberInCycle(yearNumber) - 1) + 1

  final val monthsInCycle: Int = monthsBeforeYearInCycle.last

  final def firstMonth(yearNumber: Int): Int =
    monthsInCycle*(yearCycle(yearNumber) - 1) + firstMonthInCycle(yearNumber)

  final val cycleLength: TimeInterval = Moon.meanLunarPeriod * monthsInCycle

  final def numberInCycleOfMonth(monthNumber: Int): Int = ((monthNumber - 1) % monthsInCycle) + 1

  final def monthYear(monthNumber: Int): Int = {
    val cycleOfMonth = ((monthNumber - 1) / monthsInCycle) + 1
    val yearsBeforeCycle = (cycleOfMonth - 1) * yearsInCycle
    val yearMonthIsInCycle = monthsBeforeYearInCycle.count(_ < numberInCycleOfMonth(monthNumber))
    yearsBeforeCycle + yearMonthIsInCycle
  }

  final def monthNumberInYear(monthNumber: Int): Int =
    numberInCycleOfMonth(monthNumber) - firstMonthInCycle(monthYear(monthNumber)) + 1
}
