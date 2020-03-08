package org.opentorah.calendar.astronomy

import org.opentorah.calendar.angles.Angles
import org.opentorah.calendar.angles.Angles.Rotation
import org.opentorah.calendar.numbers.{BigRational, Number}

trait InterpolatedTable[N <: Number[Angles, N]] {
  val values: Map[N, Rotation]

  private lazy val sortedValues: Seq[(N, Rotation)] = values.toSeq.sortBy(_._1)

  // KH 13:7-8, 15:7, KH 16:12
  final def interpolate(argument: N): Rotation = {
    val (allBefore: Seq[(N, Rotation)], allAfter: Seq[(N, Rotation)]) = sortedValues.span(_._1 <= argument)
    val (before /*: N*/, beforeValue: Rotation) = allBefore.last
    val reminder: Rotation = argument - before
    val more = if (reminder.isZero) Rotation.zero else {
      val (after /*: N*/, afterValue : Rotation) = allAfter.head
      val change: Rotation = afterValue - beforeValue
      val span: Rotation = after - before
      val portion: BigRational = reminder.toRational/span.toRational
      change *(portion, Angles.maxLength)
    }
    val result = beforeValue + more
    result
  }

  def calculate(argument: N): Rotation
}
