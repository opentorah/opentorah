package org.podval.calendar.angle

import org.podval.calendar.numbers.RangedHeadDigitIntervalBase
import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class AngleBase(raw: RawNumber)
  extends RangedHeadDigitIntervalBase[AngleNumberSystem](raw) with AngleNumber[AngleBase]
