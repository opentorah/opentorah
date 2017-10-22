package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem
import org.podval.calendar.angle.AngleNumberSystem.Angle
import Angle2Angle.Table
import org.podval.calendar.numbers.BigRational

abstract class Angle2Angle {
  val table: Table
}

object Angle2Angle {
  trait Table {
    def calculate(value: Angle): Angle

    val values: Map[Angle, Angle]

    final def interpolateNg(argument: Angle): Angle = {
      // TODO presort to avoid constant sorting...
      val (before: Angle, beforeValue: Angle) = values.filter(_._1 <= argument).toList.maxBy(_._1)
      val (after : Angle, afterValue : Angle) = values.filter(_._1 >  argument).toList.minBy(_._1)
      interpolate(before, beforeValue, after, afterValue)(argument)
    }

    // KH 13:7-8, 15:7, KH 16:12
    private final def interpolate(
      before: Angle,
      beforeValue: Angle,
      after: Angle,
      afterValue: Angle)(argument: Angle): Angle =
    {
      require((before <= argument) || (argument < after))
      val change: Angle = afterValue - beforeValue
      val reminder: Angle = argument - before
      val span: Angle = after - before
      val portion: BigRational = reminder.toRational/span.toRational
      val more = change*(portion, AngleNumberSystem.defaultLength)
      val result = beforeValue + more
      result
    }
  }
}
