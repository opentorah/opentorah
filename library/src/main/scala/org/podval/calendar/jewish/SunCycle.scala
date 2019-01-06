package org.podval.calendar.jewish

import Jewish.{Moment, TimeVector}
import Sun.yearOfShmuel

object SunCycle {
  val yearsInCycle: Int = 28

  // Since Birkas HaChama is said in the morning, we add 12 hours to the time of the equinox.
  // Sanctification of the Sun falls from Adar 10 to Nissan 26.
  // Only 27 days in Adar and Nissan have have the sanctification of the Sun happen on them
  // at least once.
  // It never happens on Passover.
  // It happens more often than on the Passover Eve on 7 days.
  def birkasHachama(cycle: Int): Moment =
    SeasonsFixed.Shmuel.firstTkufasNisan + yearOfShmuel * yearsInCycle * cycle + TimeVector().hours(12)

  def yearNumberInCycle(year: Jewish.Year): Int =
    (year.number - SeasonsFixed.Shmuel.firstTkufasNisan.day.year.number) % yearsInCycle + 1
}
