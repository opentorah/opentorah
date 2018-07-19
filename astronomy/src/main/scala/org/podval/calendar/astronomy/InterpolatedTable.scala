package org.podval.calendar.astronomy

import org.podval.calendar.angle.Angles.Rotation
import org.podval.calendar.numbers.BigRational

trait InterpolatedTable {
  val values: Map[Rotation, Rotation]

  // KH 13:7-8, 15:7, KH 16:12
  final def interpolate(argument: Rotation): Rotation = {
    // TODO presort to avoid constant sorting...
    val (before: Rotation, beforeValue: Rotation) = values.filter(_._1 <= argument).toList.maxBy(_._1)
    val reminder: Rotation = argument - before
    val more = if (reminder.isZero) Rotation.zero else {
      val (after : Rotation, afterValue : Rotation) = values.filter(_._1 >  argument).toList.minBy(_._1)
      val change: Rotation = afterValue - beforeValue
      val span: Rotation = after - before
      val portion: BigRational = reminder.toRational/span.toRational
      change * portion
    }
    val result = beforeValue + more
    result
  }

  def calculate(argument: Rotation): Rotation
}
