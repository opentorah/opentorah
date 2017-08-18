package org.podval.calendar.time

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.PointBase

abstract class TimePoint[T <: TimeNumberSystem[T]](raw: RawNumber)
  extends PointBase[T](raw) with TimeNumber[T, T#Point] {
  this: T#Point =>
}
