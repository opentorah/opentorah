package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle
import org.podval.calendar.astronomy.SunLongitudeCorrection.values

// TODO this is really a function Angle => Angle...
abstract class AngleCorrection {
  val values: Map[Angle, Angle]

  // KH 13:7-8, 15:7
  final def value(argument: Angle): Angle = {
    require(Angle(0) < argument && argument <= Angle(180))
    // TODO rework so that we do not need to sort it all the time...
    val vals = values.toSeq.sortBy(_._1)
    val before = vals.takeWhile(_._1 <= argument).last
    val after  = vals.find(_._1 > argument).get
    require((after._1 - before._1) == Angle(10))
    if (before._1 == argument) before._2 else {
      val more: Angle = ((after._2 - before._2) / 10)*(argument - before._1)
      before._2 + more
    }
  }

  // KH 13:?; 15:4, 15:7
  final def correction(argument: Angle): Angle = {
    val course: Angle = argument.canonical.roundTo(0) // KH 13:9
    // TODO make Angle(180) a constant on AngleNumberSystem
    val x: Boolean = course < Angle(180)
    if (course < Angle(180)) -value(course) else
    if (course > Angle(180))  value(course - Angle(180)) else
      Angle(0)
  }
}
