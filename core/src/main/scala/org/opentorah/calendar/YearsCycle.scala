package org.opentorah.calendar

trait YearsCycle:
  def length: Int

  final def yearsInCycle: Int = length

  // TODO maybe parameterize the trait itself with the Calendar?
  final def forYear(calendar: Calendar)(year: calendar.Year): YearsCycle.In = forNumber(year.number)

  def first: Int

  final def forNumber(number: Int): YearsCycle.In =
    val cycleNumber  : Int = (number - first) / length
    val numberInCycle: Int = (number - first) % length
    YearsCycle.In(
      cycleNumber   + (if numberInCycle < 0 then 0      else 1),
      numberInCycle + (if numberInCycle < 0 then length else 0) + 1
    )

  final def inCycle(cycleNumber: Int, numberInCycle: Int): Int =
    first + (cycleNumber - 1)*length + numberInCycle - 1

object YearsCycle:
  final case class In(cycleNumber: Int, numberInCycle: Int) derives CanEqual
