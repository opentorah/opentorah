package org.opentorah.calendar.jewish

import Jewish.TimeVector
import org.opentorah.calendar.YearsCycle

object LeapYearsCycle extends YearsCycle:
  final override val length: Int = 19

  final override val first: Int = 1

  final val leapYears: Set[Int] = Set(3, 6, 8, 11, 14, 17, 19)

  final val leapYearsInCycle: Int = leapYears.size

  private final def yearNumberInCycle(yearNumber: Int): Int = forNumber(yearNumber).numberInCycle

  final def isLeapYear(yearNumber: Int): Boolean = leapYears.contains(yearNumberInCycle(yearNumber))

  final def yearLengthInMonths(yearNumber: Int): Int = yearLengthInMonths(isLeapYear(yearNumber))

  private final def yearLengthInMonths(isLeap: Boolean): Int = if isLeap then 13 else 12

  final val normalYear: TimeVector = Moon.meanLunarPeriod*yearLengthInMonths(isLeap = false)

  final val leapYear: TimeVector = Moon.meanLunarPeriod*yearLengthInMonths(isLeap = true)

  private final val monthsBeforeYearInCycle: Seq[Int] = ((1 to yearsInCycle) map yearLengthInMonths).scanLeft(0)(_ + _)

  private final val monthsInCycle: Int = monthsBeforeYearInCycle.last

  private final def firstMonthInCycle(yearNumber: Int): Int = monthsBeforeYearInCycle(yearNumberInCycle(yearNumber) - 1) + 1

  final def firstMonth(yearNumber: Int): Int =
    monthsInCycle * (forNumber(yearNumber).cycleNumber - 1) + firstMonthInCycle(yearNumber)

  final val cycleLength: TimeVector = Moon.meanLunarPeriod * monthsInCycle

  // TODO make work for negative years
  private final def numberInCycleOfMonth(monthNumber: Int): Int = ((monthNumber - 1) % monthsInCycle) + 1

  // TODO make work for negative years
  final def monthYear(monthNumber: Int): Int =
    val cycleOfMonth: Int = ((monthNumber - 1) / monthsInCycle) + 1
    val yearsBeforeCycle = (cycleOfMonth - 1) * yearsInCycle
    val yearMonthIsInCycle = monthsBeforeYearInCycle.count(_ < numberInCycleOfMonth(monthNumber))
    yearsBeforeCycle + yearMonthIsInCycle

  final def monthNumberInYear(monthNumber: Int): Int =
    numberInCycleOfMonth(monthNumber) - firstMonthInCycle(monthYear(monthNumber)) + 1
