package org.podval.calendar.dates.time

import org.podval.calendar.numbers.NotRangedHeadDigitNumberSystem

abstract class TimeNumberSystem[S <: TimeNumberSystem[S]] extends {
  // TODO NumberSystem's constructor uses ranges and signs in require() calls,
  // so they need to be initialized early - but even without the require() calls
  // initialization order requires this :(

  final val hoursPerDay = 24

  final val partsPerHour = 1080

  final val momentsPerPart = 76

  final override val ranges: List[Int] = List(hoursPerDay, partsPerHour, momentsPerPart)

  final override val signs: List[String] = List("d", "h", "p", "m")

} with NotRangedHeadDigitNumberSystem[S] { this: S =>
  require(hoursPerDay % 2 == 0)

  final val hoursPerHalfDay: Int = hoursPerDay / 2

  final val minutesPerHour = 60

  require(partsPerHour % minutesPerHour == 0)

  final val partsPerMinute: Int = partsPerHour / minutesPerHour

  type Point <: TimePointBase[S]

  type Interval <: TimeIntervalBase[S]
}
