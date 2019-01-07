package org.podval.calendar.dates

trait Cycle {
  def length: Int

  def first: Int

  final def forNumber(number: Int): Cycle.In = Cycle.In(
    (number - first) / length + 1,
    (number - first) % length + 1
  )

  final def inCycle(cycleNumber: Int, numberInCycle: Int): Int =
    first + (cycleNumber - 1)*length + numberInCycle - 1
}

object Cycle {
  final case class In(cycleNumber: Int, numberInCycle: Int)
}
