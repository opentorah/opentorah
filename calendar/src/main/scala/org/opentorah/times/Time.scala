package org.opentorah.times

import Times.{hoursPerHalfDay, partsPerHalfHour, partsPerMinute}
import org.opentorah.numbers.Number

trait Time[S <: Times[S], N <: Time[S, N]] extends Number[S, N] { this: N =>
  private def Digit: Times.Digit.type = Times.Digit

  final def days: Int = get(Digit.DAYS)

  final def days(value: Int): N = set(Digit.DAYS, value)

  final def day(number: Int): N = days(number-1)

  final def time: S#Vector = this - companion(days)

  final def hours: Int = get(Digit.HOURS)

  final def hours(value: Int): N = set(Digit.HOURS, value)

  final def firstHalfHours(value: Int): N = {
    require(0 <= hours && hours < hoursPerHalfDay)
    hours(value)
  }

  final def secondHalfHours(value: Int): N = {
    require(0 <= value && value < hoursPerHalfDay)
    hours(value + hoursPerHalfDay)
  }

  final def parts: Int = get(Digit.PARTS)

  final def parts(value: Int): N = set(Digit.PARTS, value)

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

  final def moments: Int = get(Digit.MOMENTS)

  final def moments(value: Int): N = set(Digit.MOMENTS, value)
}
