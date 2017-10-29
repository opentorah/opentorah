package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem
import org.podval.calendar.angle.AngleNumberSystem.Angle
import org.podval.calendar.numbers.BigRational

trait InterpolatedTable {
  val values: Map[Angle, Angle]

  // KH 13:7-8, 15:7, KH 16:12
  final def interpolate(argument: Angle): Angle = {
    // TODO presort to avoid constant sorting...
    val (before: Angle, beforeValue: Angle) = values.filter(_._1 <= argument).toList.maxBy(_._1)
    val reminder: Angle = argument - before
    val more = if (reminder.isZero) {
      Angle.zero
    } else {
      val (after : Angle, afterValue : Angle) = values.filter(_._1 >  argument).toList.minBy(_._1)
      val change: Angle = afterValue - beforeValue
      val span: Angle = after - before
      val portion: BigRational = reminder.toRational/span.toRational
      change*(portion, AngleNumberSystem.defaultLength)
    }
    val result = beforeValue + more
    result
  }

  def calculate(argument: Angle): Angle
}
