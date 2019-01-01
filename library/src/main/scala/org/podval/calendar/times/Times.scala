package org.podval.calendar.times

import org.podval.calendar.numbers.NonPeriodicNumbers

trait Times[S <: Times[S]] extends NonPeriodicNumbers[S] { this: S =>
  type Point <: TimePointBase[S]

  type Vector <: TimeVectorBase[S]

  final override def range(position: Int): Int = position match {
    case 0 => Times.hoursPerDay
    case 1 => Times.partsPerHour
    case 2 => Times.momentsPerPart
  }

  final override def headSign: String = "d"

  final override val signPartial: PartialFunction[Int, String] = {
    case 0 => "h"
    case 1 => "p"
    case 2 => "m"
  }

  final override val defaultLength: Int = 3

  val week: S#Vector = Vector().days(7)
}


object Times {
  final val hoursPerDay: Int = 24
  require(hoursPerDay % 2 == 0)

  final val hoursPerHalfDay: Int = hoursPerDay / 2

  final val partsPerHour: Int = 1080

  final val partsPerHalfHour: Int = partsPerHour / 2

  final val minutesPerHour: Int = 60 // KH 10:1
  require(partsPerHour % minutesPerHour == 0)

  final val partsPerMinute: Int = partsPerHour / minutesPerHour

  final val momentsPerPart: Int = 76

  final val momentsPerMinute: Int = partsPerMinute*momentsPerPart

  final val secondsPerMinute: Int = 60

  final val millisecondsPerSecond: Int = 1000

  final val millisecondsPerMinute: Int = secondsPerMinute*millisecondsPerSecond
}
