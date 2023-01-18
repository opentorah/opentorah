package org.opentorah.astronomy

import org.opentorah.astronomy
import org.opentorah.astronomy.Angles.Rotation
import org.opentorah.numbers.BigRational

abstract class OrderedRotationTable[N <: Angles.Angle[N]](values: (String, String)*)(s2n: String => N)
  extends OrderedTable[String, N, Rotation](values*)(s2n, Rotation(_)):

  // KH 13:7-8, 15:7, 16:12
  final def interpolate(argument: N): Rotation =
    val (it: (N, Rotation), next: Option[(N, Rotation)]) = bracket(argument)
    val (before: N, beforeValue: Rotation) = it
    if (argument-before).isZero then beforeValue else // TODO where is a my CanEqual so that I can do argument == before?!
      val (after : N, afterValue : Rotation) = next.get
      val portion: BigRational = (argument - before).toRational / (after - before).toRational
      beforeValue + (afterValue - beforeValue) * (portion, Angles.maxLength)

  def calculate(argument: N): Rotation
