package org.podval.calendar.angle

import org.podval.calendar.numbers.IntervalBase
import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class AngleBase(raw: RawNumber)
  extends IntervalBase[AngleNumberSystem](raw) with AngleNumber[AngleBase]
