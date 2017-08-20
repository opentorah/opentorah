package org.podval.calendar.astronomy.angle

class AnglePointCompanion {
  final def apply(negative: Boolean, digits: List[Int]): AnglePoint =
    AngleNumberSystem.createPoint(negative, digits)
}
