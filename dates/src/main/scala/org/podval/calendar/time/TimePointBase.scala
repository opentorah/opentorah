package org.podval.calendar.time

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.PointBase

abstract class TimePointBase[S <: TimeNumberSystem[S]](raw: RawNumber)
  extends PointBase[S](raw) with TimeNumber[S, S#Point]
{
  this: S#Point =>
}
