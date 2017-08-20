package org.podval.calendar.astronomy.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.PointBase

// TODO rename AnglePointBase?
abstract class AnglePoint(raw: RawNumber)
  extends PointBase[AngleNumberSystem](raw) with AngleNumber[AnglePoint]
