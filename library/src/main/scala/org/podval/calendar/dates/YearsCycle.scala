package org.podval.calendar.dates

trait YearsCycle {
  def length: Int

  final def yearsInCycle: Int = length

  final def forYear(year: YearBase[_]): YearsCycle.In = forNumber(year.number)

  def first: Int

  final def forNumber(number: Int): YearsCycle.In = YearsCycle.In(
    (number - first) / length + 1,
    (number - first) % length + 1
  )

  final def inCycle(cycleNumber: Int, numberInCycle: Int): Int =
    first + (cycleNumber - 1)*length + numberInCycle - 1
}

object YearsCycle {
  final case class In(cycleNumber: Int, numberInCycle: Int)
}
