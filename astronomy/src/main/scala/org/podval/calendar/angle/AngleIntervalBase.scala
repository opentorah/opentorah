package org.podval.calendar.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.RangedHeadDigitInterval

// TODO rename AngleInterval
abstract class AngleIntervalBase(raw: RawNumber)
  extends RangedHeadDigitInterval[AngleNumberSystem](raw) with AngleNumber[AngleIntervalBase]
