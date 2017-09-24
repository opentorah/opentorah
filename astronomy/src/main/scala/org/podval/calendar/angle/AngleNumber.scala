package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicNumber

trait AngleNumber[N <: AngleNumber[N]] extends PeriodicNumber[AngleNumberSystem, N]
{ this: N =>
  def degrees: Int = head

  def degrees(value: Int): N = head(value)

  def roundToDegrees: N = roundTo(0)

  def minutes: Int = tail(0)

  def minutes(value: Int): N = tail(0, value)

  def roundToMinutes: N = roundTo(1)

  def seconds: Int = tail(1)

  def seconds(value: Int): N = tail(1, value)

  def roundToSeconds: N = roundTo(2)

  def thirds: Int  = tail(2)

  def thirds(value: Int): N = tail(2, value)

  def toRadians: Double = math.toRadians(toDegrees)

  def toDegrees: Double = toDouble
}
