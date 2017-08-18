package org.podval.calendar.calendar

import org.podval.calendar.numbers.IntervalBase
import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class TimeInterval[T <: TimeNumberSystem[T]](raw: RawNumber)
  extends IntervalBase[T](raw) with TimeNumber[T, T#Interval] {
  this: T#Interval =>
}
