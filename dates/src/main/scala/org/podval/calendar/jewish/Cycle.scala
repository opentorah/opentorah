package org.podval.calendar.jewish

import Jewish.{Month, TimeInterval}

object Cycle {
  final val yearsInCycle: Int = 19

  final val leapYears: Set[Int] =
    Set(3, 6, 8, 11, 14, 17, 19) // TODO calculate Meton's cycle in the paper

  final val leapYearsInCycle: Int = leapYears.size

  final def isLeap(yearNumber: Int): Boolean =
    leapYears.contains(Cycle.numberInCycle(yearNumber))

  final def cycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1

  final def numberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1

  final def lengthInMonths(yearNumber: Int): Int = lengthInMonths(isLeap(yearNumber))

  final def lengthInMonths(isLeap: Boolean): Int = if (isLeap) 13 else 12

  final val monthsBeforeYearInCycle: Seq[Int] =
    ((1 to yearsInCycle) map lengthInMonths).scanLeft(0)(_ + _)

  final def firstMonthInCycle(yearNumber: Int): Int =
    monthsBeforeYearInCycle(numberInCycle(yearNumber) - 1) + 1

  final val monthsInCycle: Int = monthsBeforeYearInCycle.last

  final def firstMonth(yearNumber: Int): Int =
    monthsInCycle*(cycle(yearNumber) - 1) + firstMonthInCycle(yearNumber)

  final val cycleLength: TimeInterval = Month.meanLunarPeriod * monthsInCycle

  final def numberInCycleOfMonth(monthNumber: Int): Int = ((monthNumber - 1) % monthsInCycle) + 1

  final def yearNumber(monthNumber: Int): Int = {
    val cycleOfMonth = ((monthNumber - 1) / monthsInCycle) + 1
    val yearsBeforeCycle = (cycleOfMonth - 1) * yearsInCycle
    val yearMonthIsInCycle = monthsBeforeYearInCycle.count(_ < numberInCycleOfMonth(monthNumber))
    yearsBeforeCycle + yearMonthIsInCycle
  }

  final def numberInYear(monthNumber: Int): Int =
    numberInCycleOfMonth(monthNumber) - firstMonthInCycle(yearNumber(monthNumber)) + 1
}
