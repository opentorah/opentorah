package org.podval.calendar.angle

import org.podval.calendar.numbers.RangedHeadDigitIntervalBase
import org.podval.calendar.numbers.NumberSystem.RawNumber

// TODO rename AngleInterval
abstract class AngleIntervalBase(raw: RawNumber)
  extends RangedHeadDigitIntervalBase[AngleNumberSystem](raw) with AngleNumber[AngleIntervalBase]
