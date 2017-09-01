package org.podval.calendar.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.RangedHeadDigitPoint

// TODO rename AnglePoint - when there is no AngleNumberSystem.AnglePoint
abstract class AnglePointBase(raw: RawNumber)
  extends RangedHeadDigitPoint[AngleNumberSystem](raw) with AngleNumber[AnglePointBase]
