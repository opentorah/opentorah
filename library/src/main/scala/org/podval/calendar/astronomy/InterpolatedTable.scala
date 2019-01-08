package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles
import org.podval.calendar.angles.Angles.Rotation
import org.podval.calendar.numbers.{BigRational, Number}

trait InterpolatedTable[N <: Number[Angles, N]] {
  val values: Map[N, Rotation]

  // KH 13:7-8, 15:7, KH 16:12
  final def interpolate(argument: N): Rotation = {
    // TODO presort to avoid constant sorting...
    val (before /*: N*/, beforeValue: Rotation) = values.filter(_._1 <= argument).toList.maxBy(_._1)
    val reminder: Rotation = argument - before
    val more = if (reminder.isZero) Rotation.zero else {
      val (after /*: N*/, afterValue : Rotation) = values.filter(_._1 >  argument).toList.minBy(_._1)
      val change: Rotation = afterValue - beforeValue
      val span: Rotation = after - before
      val portion: BigRational = reminder.toRational/span.toRational
      change * portion
    }
    val result = beforeValue + more
    result
  }

  def calculate(argument: N): Rotation
}
