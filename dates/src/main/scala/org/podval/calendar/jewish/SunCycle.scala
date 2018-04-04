package org.podval.calendar.jewish

import Jewish.{Moment, TimeInterval}
import Sun.yearOfShmuel

// TODO Where and when was the Sun created? Does this jibe with Rambam's epoch?
object SunCycle {
  // Since Birkas HaChama is said in the morning, we add 12 hours to the time of the equinox.
  // Sanctification of the Sun falls from Adar 10 to Nissan 26.
  // Only 27 days in Adar and Nissan have have the sanctification of the Sun happen on them
  // at least once.
  // It never happens on Passover.
  // It happens more often than on the Passover Eve on 7 days.
  def birkasHachama(cycle: Int): Moment =
    SeasonsFixed.Shmuel.firstTkufasNisan + yearOfShmuel * 28 * cycle + TimeInterval().hours(12)
}
