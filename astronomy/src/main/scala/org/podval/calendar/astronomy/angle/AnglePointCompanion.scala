package org.podval.calendar.astronomy.angle

import AngleNumberSystem.AnglePoint

class AnglePointCompanion {
  final def apply(negative: Boolean, digits: List[Int]): AnglePoint =
    AngleNumberSystem.createPoint(negative, digits)
}
