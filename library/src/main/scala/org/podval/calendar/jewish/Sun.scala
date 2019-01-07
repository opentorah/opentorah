package org.podval.calendar.jewish

import org.podval.calendar.dates.YearsCycle
import Jewish.{Moment, TimeVector, Year}

abstract class Sun extends Season.ForYear {
  def yearLength: TimeVector

  final def seasonLength: TimeVector = yearLength / 4

  final def firstTkufasNisan: Moment =
    Moon.firstMoladNisan - firstTkufasNisanBeforeFirstMoladNisan

  def firstTkufasNisanBeforeFirstMoladNisan: TimeVector

  final override def seasonForYear(season: Season, year: Year): Moment = {
    val numberInYear: Int = season.numberInYear - Season.TkufasNisan.numberInYear
    seasonForYear(if (numberInYear >= 0) numberInYear else numberInYear + 4, year)
  }

  // This method calculates tkufas Tishrei and Teves of year n in year n, before tkufas Nisan,
  // not year n+1 like in the Rambam's text.
  final def seasonForYearFromTishrei(season: Season, year: Year): Moment =
    seasonForYear(season.numberInYear - Season.TkufasNisan.numberInYear, year)

  private def seasonForYear(number: Int, year: Year): Moment = firstTkufasNisan +
    seasonLength * ((year.number - 1) * Season.numberOf + number)
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

    final override val first: Int = firstTkufasNisan.day.year.number

    // Since Birkas HaChama is said in the morning, we add 12 hours to the time of the equinox.
    // Sanctification of the Sun falls from Adar 10 to Nissan 26.
    // Only 27 days in Adar and Nissan have have the sanctification of the Sun happen on them
    // at least once.
    // It never happens on Passover.
    // It happens more often than on the Passover Eve on 7 days.
    def birkasHachama(cycle: Int): Moment =
      firstTkufasNisan + yearLength * length * cycle + TimeVector().hours(12)
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
