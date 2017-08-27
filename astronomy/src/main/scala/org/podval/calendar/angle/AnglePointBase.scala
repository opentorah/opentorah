package org.podval.calendar.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.PointBase

abstract class AnglePointBase(raw: RawNumber)
  extends PointBase[AngleNumberSystem](raw) with AngleNumber[AnglePointBase]
