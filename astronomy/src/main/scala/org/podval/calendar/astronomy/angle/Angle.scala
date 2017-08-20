package org.podval.calendar.astronomy.angle

import org.podval.calendar.numbers.IntervalBase
import org.podval.calendar.numbers.NumberSystem.RawNumber

// TODO rename AngleBase?
abstract class Angle(raw: RawNumber)
  extends IntervalBase[AngleNumberSystem](raw) with AngleNumber[Angle]
