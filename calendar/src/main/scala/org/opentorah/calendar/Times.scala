package org.opentorah.calendar

import org.opentorah.numbers.{Digit, Digits, DigitsDescriptor, NonPeriodicNumbers}

trait Times extends NonPeriodicNumbers {

  import Times.{hoursPerHalfDay, partsPerHalfHour, partsPerMinute}

  trait Time[N <: Time[N]] extends Number[N] { this: N =>
    private def Digit: TimesDigits.type = TimesDigits

    final def days: Int = get(Digit.DAYS)

    final def days(value: Int): N = set(Digit.DAYS, value)

    final def time: Vector = this - companion(days)

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

  class TimePointBase(digits: Digits) extends PointNumber(digits) with Time[Point] { this: Point => }

  override type Point <: TimePointBase

  final class TimeVectorBase(digits: Digits) extends VectorNumber(digits) with Time[Vector] { this: Vector => }

  final override type Vector = TimeVectorBase

  final override type VectorCompanionType = VectorCompanion

  final override protected def createVectorCompanion: VectorCompanionType = new VectorCompanion

  final override protected def newVector(digits: Seq[Int]): Vector = new TimeVectorBase(digits)

  final override val maxLength: Int = 3

  final override def range(position: Int): Int = position match {
    case 0 => Times.hoursPerDay
    case 1 => Times.partsPerHour
    case 2 => Times.momentsPerPart
  }

  final override type DigitType = DigitsDescriptor

  final override protected def createDigit: DigitsDescriptor = TimesDigits
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
