package org.podval.calendar.dates

trait YearsCycle extends Cycle {
  final def yearsInCycle: Int = length

  final def forYear(year: YearBase[_]): Cycle.In = forNumber(year.number)
}
