package org.podval.calendar.time

import org.podval.calendar.numbers.Number

// TODO when this and Number are inside their familty type (and not split):
// if N <: TimeNumber[T, N], I get compiler error;
// if I change it to T#TimeNumber[T, N] - which is what I want,
//  and which I did in Number - I get compiler crash!!!
trait TimeNumber[T <: TimeNumberSystem[T], N <: TimeNumber[T, N]] extends Number[T, N] { this: N =>
  private[this] def hoursPerHalfDay = numberSystem.hoursPerHalfDay
  private[this] def partsPerMinute = numberSystem.partsPerMinute

  final def days: Int = head

  final def days(value: Int): N = digit(0, value)

  final def day(number: Int): N = days(number-1)

  final def hours: Int = digit(1)

  final def hours(value: Int): N = digit(1, value)

  final def firstHalfHours(value: Int): N = {
    require(0 <= hours && hours < hoursPerHalfDay)
    hours(value)
  }

  final def secondHalfHours(value: Int): N = {
    require(0 <= value && value < hoursPerHalfDay)
    hours(value + hoursPerHalfDay)
  }

  final def parts: Int = digit(2)

  final def parts(value: Int): N = digit(2, value)

  final def minutes: Int = parts / partsPerMinute

  final def minutes(value: Int): N = parts(value*partsPerMinute+partsWithoutMinutes)

  final def partsWithoutMinutes: Int = parts % partsPerMinute

  final def partsWithoutMinutes(value: Int): N = parts(minutes*partsPerMinute+value)

  final def moments: Int = digit(3)

  final def moments(value: Int): N = digit(3, value)
}
