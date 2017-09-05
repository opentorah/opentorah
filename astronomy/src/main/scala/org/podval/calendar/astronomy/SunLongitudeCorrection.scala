package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle

// KH 13:4
// TODO there are some diagrams in the Moznaim Rambam at this point.
object SunLongitudeCorrection {
  private final val values: Seq[(Angle, Angle)] = Seq(
    Angle( 10) -> Angle(0, 20),
    Angle( 20) -> Angle(0, 40),
    Angle( 30) -> Angle(0, 58),
    Angle( 40) -> Angle(1, 15),
    Angle( 50) -> Angle(1, 29),
    Angle( 60) -> Angle(1, 41),
    Angle( 70) -> Angle(1, 51),
    Angle( 80) -> Angle(1, 57),
    Angle( 90) -> Angle(1, 59),
    Angle(100) -> Angle(1, 58),
    Angle(110) -> Angle(1, 53),
    Angle(120) -> Angle(1, 45),
    Angle(130) -> Angle(1, 33),
    Angle(140) -> Angle(1, 19),
    Angle(150) -> Angle(1,  1),
    Angle(160) -> Angle(0, 42),
    Angle(170) -> Angle(0, 21),
    Angle(180) -> Angle(0,  0)
  )

  // KH 13:7-8
  final def value(course: Angle): Angle = {
    require(Angle(0) < course && course < Angle(180))
    val before = values.takeWhile(_._1 <= course).last
    val after  = values.find(_._1 > course).get
    require((after._1 - before._1) == Angle(10))
    if (before._1 == course) before._2 else {
      val more: Angle = ((after._2 - before._2) / 10)*(course - before._1)
      before._2 + more
    }
  }

  // KH 13:?
  final def correction(rawCourse: Angle): Angle = {
    val course: Angle = rawCourse.canonical.roundTo(0) // KH 13:9
    // TODO make Angle(180) a constant on AngleNumberSystem
    val x: Boolean = course < Angle(180)
    if (course < Angle(180)) -value(course) else
    if (course > Angle(180))  value(course - Angle(180)) else
      Angle(0)
  }
}
