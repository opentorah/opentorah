package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicNumber

trait Angle[N <: Angle[N]] extends PeriodicNumber[Angles, N] { this: N =>
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

  def roundToThirds: N = roundTo(3)

  def toRadians: Double = math.toRadians(toDegrees)

  def toDegrees: Double = toDouble
}
