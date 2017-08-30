package org.podval.calendar.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.RangedHeadDigitPointBase

abstract class AnglePointBase(raw: RawNumber)
  extends RangedHeadDigitPointBase[AngleNumberSystem](raw) with AngleNumber[AnglePointBase]
