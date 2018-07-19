package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicVector

// TODO rename AngleBase if AngleVector stays Angle
abstract class AngleVectorBase(digits: Seq[Int])
  extends PeriodicVector[AngleNumberSystem](digits) with AngleNumber[AngleVectorBase]
