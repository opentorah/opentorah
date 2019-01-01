package org.podval.calendar.times

import org.podval.calendar.numbers.NonPeriodicNumber
import Times.{hoursPerHalfDay, partsPerHalfHour, partsPerMinute}

trait Time[S <: Times[S], N <: Time[S, N]] extends NonPeriodicNumber[S, N] { this: N =>
  final def days: Int = head

  final def days(value: Int): N = head(value)

  final def day(number: Int): N = days(number-1)

  final def time: S#Vector = days(0).toVector

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

  final def minutes(value: Int): N = parts(value*partsPerMinute + partsWithoutMinutes)

  final def partsWithoutMinutes: Int = parts % partsPerMinute

  final def partsWithoutMinutes(value: Int): N = parts(minutes*partsPerMinute + value)

  final def seconds: Int = (partsWithoutMinutes*Times.momentsPerPart + moments) * Times.secondsPerMinute /
    Times.momentsPerMinute

  final def milliseconds: Int = ((partsWithoutMinutes*Times.momentsPerPart + moments) * Times.secondsPerMinute %
    Times.momentsPerMinute) * Times.millisecondsPerSecond / Times.momentsPerMinute

  final def secondsAndMilliseconds(seconds: Int, milliseconds: Int): N = {
    val units: Int = (seconds*Times.millisecondsPerSecond + milliseconds)*Times.partsPerMinute
    val newParts: Int = units / Times.millisecondsPerMinute
    val newMoments: Int = (units % Times.millisecondsPerMinute)*Times.momentsPerPart / Times.millisecondsPerMinute
    partsWithoutMinutes(newParts).moments(newMoments)
  }

  final def moments: Int = tail(2)

  final def moments(value: Int): N = tail(2, value)
}
