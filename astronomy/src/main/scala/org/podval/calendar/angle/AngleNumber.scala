package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicNumber

trait AngleNumber[N <: AngleNumber[N]] extends PeriodicNumber[AngleNumberSystem, N]
{ this: N =>
  def degrees: Int = head

  def degrees(value: Int): N = digit(0, value)

  def minutes: Int = digit(1)

  def minutes(value: Int): N = digit(1, value)

  def roundToMinutes: N = roundTo(1)

  def seconds: Int = digit(2)

  def seconds(value: Int): N = digit(2, value)

  def roundToSeconds: N = roundTo(2)

  def thirds: Int  = digit(3)

  def thirds(value: Int): N = digit(3, value)

  def toRadians: Double = math.toRadians(toDegrees)

  def toDegrees: Double = toDouble
}
