package org.podval.calendar.angle

import AngleNumberSystem.AnglePoint

class AnglePointCompanion {
  final def apply(negative: Boolean, digits: List[Int]): AnglePoint =
    AngleNumberSystem.newPoint(negative, digits)
}
