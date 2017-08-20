package org.podval.calendar.astronomy.moon

import scala.collection.immutable.Map
import org.podval.calendar.astronomy.angle.AngleNumberSystem.Angle
import org.podval.calendar.astronomy.angle.Angle
import org.podval.calendar.astronomy.angle.AngleCompanion.angleToRadians
import scala.math.{sin, cos, asin, sqrt, abs, pow, round}


object AnomalyVisible {
    /* Numbers as they are printed in Law 15:6 */
    val MISPRINTED: Map[Angle, Angle] = Map(
        Angle( 10) -> Angle(0, 50),
        Angle( 20) -> Angle(1, 38),
        Angle( 30) -> Angle(2, 24),
        Angle( 40) -> Angle(3,  6),
        Angle( 50) -> Angle(3, 44),
        Angle( 60) -> Angle(4, 16),
        Angle( 70) -> Angle(4, 41),
        Angle( 80) -> Angle(5,  0),
        Angle( 90) -> Angle(5,  5),
        Angle(100) -> Angle(5,  8),
        Angle(110) -> Angle(4, 59),
        Angle(120) -> Angle(4, 20),
        Angle(130) -> Angle(4, 14),
        Angle(140) -> Angle(3, 33),
        Angle(150) -> Angle(3, 48),
        Angle(160) -> Angle(1, 56),
        Angle(170) -> Angle(1, 59)
    )

    val VALUES: Map[Angle, Angle] = MISPRINTED ++ Map(
        Angle(120) -> Angle(4, 40),
        Angle(150) -> Angle(2, 48),
        Angle(170) -> Angle(0, 59)
    )

    def mnasfrome(maslul: Angle, e: Double): Angle = {
      val inRadians = asin(sin(maslul)/sqrt(e*e + 2*e*cos(maslul) + 1))
      Angle.fromRadians(inRadians, 1)
    }

    def efrommnas(maslul: Angle, mnas: Angle): Double = sin(maslul)/sin(mnas)*abs(cos(mnas))-cos(maslul)

    def efrommnasround(maslul: Angle, mnas: Angle): Double = roundTo(efrommnas(maslul, mnas), 2)

    private def roundTo(value: Double, digits: Int): Double = {
        val quotient = pow(10, digits)
        round(value*quotient)/quotient
    }
}
