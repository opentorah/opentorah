package org.podval.calendar.calendar

import org.podval.calendar.numbers.NotRangedHeadDigitNumberSystem
import org.podval.calendar.numbers.NumberSystem.RawNumber

// TODO before I can derive Calendar from this, it has to sprout a type parameter
// [S <: TimeNumberSytem[S]]...
abstract class TimeNumberSystem extends {
  // TODO NumberSystem's constructor uses ranges and signs in require() calls,
  // so they need to be initialized early - but even without the require() calls
  // initialization order requires this :(

  final val hoursPerDay = 24

  final val partsPerHour = 1080

  final val momentsPerPart = 76

  final override val ranges: List[Int] = List(hoursPerDay, partsPerHour, momentsPerPart)

  final override val signs: List[String] = List("d", "h", "p", "m")

} with NotRangedHeadDigitNumberSystem[TimeNumberSystem] {
  require(hoursPerDay % 2 == 0)

  final val hoursPerHalfDay: Int = hoursPerDay / 2

  final val minutesPerHour = 60

  require(partsPerHour % minutesPerHour == 0)

  final val partsPerMinute: Int = partsPerHour / minutesPerHour

  type Point <: TimePoint

  abstract class TimePoint(raw: RawNumber) extends PointBase(raw) with TimeNumber[Point] {
    this: Point =>
  }

  type Interval <: TimeInterval

  // TODO make abstract just like TimePoint?
  final class TimeInterval(raw: RawNumber) extends IntervalBase(raw) with TimeNumber[Interval] {
    this: Interval =>
  }

  trait TimeNumber[N <: TimeNumber[N]] extends Number[N] { this: N =>
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
}
