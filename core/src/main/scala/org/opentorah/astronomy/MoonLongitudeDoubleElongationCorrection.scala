package org.opentorah.astronomy

import Angles.Rotation

object MoonLongitudeDoubleElongationCorrection:
  // KH 15:2-3
  def calculate(doubleElongation: Rotation): Rotation =
    def between(from: Int, to: Int): Boolean =
      (Rotation(from) <= doubleElongation) && (doubleElongation < Rotation(to + 1))

    val result: Int =
      if between( 0,  5) then 0 else
      if between( 6, 11) then 1 else
      if between(12, 18) then 2 else
      if between(19, 24) then 3 else
      if between(25, 31) then 4 else
      if between(32, 38) then 5 else
      if between(39, 45) then 6 else
      if between(46, 51) then 7 else
      if between(52, 59) then 8 else
      if between(60, 63) then 9 else
        0 // so that Calculator doesn't throw exceptions

    Rotation(result)
