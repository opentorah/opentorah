package org.podval.calendar.jewish

import org.podval.calendar.dates.YearsCycle
import Jewish.{Moment, TimeVector, Year}

abstract class Sun extends Seasons {
  final def tkufasNisan  (year: Year): Moment = tkufa(year, 0)
  final def tkufasTammuz (year: Year): Moment = tkufa(year, 1)
  final def tkufasTishrei(year: Year): Moment = tkufa(year, 2)
  final def tkufasTeves  (year: Year): Moment = tkufa(year, 3)

  final def tkufa(year: Year, number: Int): Moment =
    firstTkufasNisan + seasonLength * ((year.number - 1)*4 + number)

  def yearLength: TimeVector

  final def seasonLength: TimeVector = yearLength / 4

  final def firstTkufasNisan: Moment =
    Moon.firstMoladNisan - firstTkufasNisanBeforeFirstMoladNisan

  val firstTkufasNisanBeforeFirstMoladNisan: TimeVector
}

object Sun {
  object Shmuel extends Sun with YearsCycle {
    // KH 9:1
    final override val yearLength: TimeVector =
      TimeVector().days(365).hours(6)

    // KH 9:3
    final override val firstTkufasNisanBeforeFirstMoladNisan: TimeVector =
      TimeVector().days(7).hours(9).parts(642)

    final override val length: Int = 28

    private val start: Jewish.Moment = firstTkufasNisan

    final override val first: Int = start.day.year.number

    // Since Birkas HaChama is said in the morning, we add 12 hours to the time of the equinox.
    // Sanctification of the Sun falls from Adar 10 to Nissan 26.
    // Only 27 days in Adar and Nissan have have the sanctification of the Sun happen on them
    // at least once.
    // It never happens on Passover.
    // It happens more often than on the Passover Eve on 7 days.
    def birkasHachama(cycle: Int): Moment = start + yearLength * length * cycle + TimeVector().hours(12)
  }

  object RavAda extends Sun {
    // KH 10:1
    final override val yearLength: TimeVector =
      LeapYearsCycle.cycleLength / LeapYearsCycle.yearsInCycle

    // KH 10:3
    final override val firstTkufasNisanBeforeFirstMoladNisan: TimeVector =
      TimeVector().hours(9).parts(642)
  }
}
