package org.podval.calendar.time

import org.podval.calendar.numbers.Number
import TimeNumberSystem.{hoursPerHalfDay, partsPerHalfHour, partsPerMinute}

trait TimeNumber[S <: TimeNumberSystem[S], N <: TimeNumber[S, N]] extends Number[S, N] { this: N =>
  final def days: Int = head

  final def days(value: Int): N = head(value)

  final def day(number: Int): N = days(number-1)

  final def time: S#Interval = days(0).toInterval

  final def hours: Int = tail(0)

  final def hours(value: Int): N = tail(0, value)

  final def firstHalfHours(value: Int): N = {
    require(0 <= hours && hours < hoursPerHalfDay)
    hours(value)
  }

  final def secondHalfHours(value: Int): N = {
    require(0 <= value && value < hoursPerHalfDay)
    hours(value + hoursPerHalfDay)
  }

  final def parts: Int = tail(1)

  final def parts(value: Int): N = tail(1, value)

  final def halfHour: N = parts(partsPerHalfHour)

  final def minutes: Int = parts / partsPerMinute

  final def minutes(value: Int): N = parts(value*partsPerMinute+partsWithoutMinutes)

  final def partsWithoutMinutes: Int = parts % partsPerMinute

  final def partsWithoutMinutes(value: Int): N = parts(minutes*partsPerMinute+value)

  final def moments: Int = tail(2)

  final def moments(value: Int): N = tail(2, value)
}
