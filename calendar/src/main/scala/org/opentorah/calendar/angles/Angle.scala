package org.opentorah.calendar.angles

import org.opentorah.calendar.numbers.Number
import Angles.Digit

trait Angle[N <: Angle[N]] extends Number[Angles, N] { this: N =>
  def degrees: Int = get(Digit.DEGREES)

  def degrees(value: Int): N = set(Digit.DEGREES, value)

  def roundToDegrees: N = roundTo(Digit.DEGREES)

  def minutes: Int = get(Digit.MINUTES)

  def minutes(value: Int): N = set(Digit.MINUTES, value)

  def roundToMinutes: N = roundTo(Digit.MINUTES)

  def seconds: Int = get(Digit.SECONDS)

  def seconds(value: Int): N = set(Digit.SECONDS, value)

  def roundToSeconds: N = roundTo(Digit.SECONDS)

  def thirds: Int  = get(Digit.THIRDS)

  def thirds(value: Int): N = set(Digit.THIRDS, value)

  def roundToThirds: N = roundTo(Digit.THIRDS)

  def toRadians: Double = math.toRadians(toDegrees)

  def toDegrees: Double = toDouble
}
