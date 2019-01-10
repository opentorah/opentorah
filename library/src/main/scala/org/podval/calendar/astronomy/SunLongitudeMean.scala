package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles
import org.podval.calendar.angles.Angles.Rotation
import org.podval.calendar.jewish.{Jewish, Sun}
import org.podval.calendar.numbers.BigRational

object SunLongitudeMean extends Days2Angle {
  // KH 12:1
  final override val one        : Rotation = Rotation(  0, 59,  8)
  final override val ten        : Rotation = Rotation(  9, 51, 23)
  final override val hundred    : Rotation = Rotation( 98, 33, 53)
  final override val thousand   : Rotation = Rotation(265, 38, 50) // remainder
  final override val tenThousand: Rotation = Rotation(136, 28, 20)

  // TODO reconstructs if calculated as 3*v(10)-v(1); not if using the exact value :)
  final override val month      : Rotation = Rotation( 28, 35,  1)
  final override val year       : Rotation = Rotation(348, 55, 15)  // TODO ??

  final override def rounder(key: Days2Angle.Key): Rotation => Rotation = _.roundToSeconds

  // Ibn Habib on Pirush gives Albatani value as:
  final val albataniValue: Rotation = Rotation(0, 59, 8, 20, 35)

  // Moznaim Rambam in English gives this value in KH 12:1 note 1 (without a reference) as the one
  // Rambam uses in his calculations.
  // TODO which of the year lengths does this correspond?
  final override val rambamValue: Rotation = Rotation(0, 59, 8, 19, 48)

  final override val almagestValue: Rotation = Rotation(0, 59, 8, 17, 13, 12, 31)


  // TODO move into tests
  def main(args: Array[String]) {
    def m(n: Int) = (rambamValue * n).roundTo(Angles.Digit.SECONDS)
    for (n <- List(1, 10, 100, 1000, 10000, 354))
      println(n + " " + m(n))

    val v29 = Rotation(9, 51, 23)*3 - Rotation(0, 59, 8)
    println(v29)

    val v354 = Rotation(98, 33, 53)*3 + Rotation(9, 51, 23)*5 + Rotation(0, 59, 8)*4
    println(v354)

    def rat(rot: Rotation): BigRational = Rotation(360).toRational/rot.toRational
    def len(x: BigRational): Jewish.TimeVector = Jewish.TimeVector.fromRational(x, 3)

    val albatani = rat(albataniValue)
    val moznaim  = rat(rambamValue)
    val almagest = rat(almagestValue)

    println("Albatani: " + albatani.toDouble + " " + len(albatani))
    println("Moznaim : " + moznaim.toDouble + " " + len(moznaim))
    println("Almagest: " + almagest.toDouble + " " + len(almagest))
    println("Rav Ada : " + Sun.RavAda.yearLength.toRational.toDouble + " " + Sun.RavAda.yearLength)
    println("Shmuel  : " + Sun.Shmuel.yearLength.toRational.toDouble + " " + Sun.Shmuel.yearLength)

    println((almagestValue*1000).roundToMinutes)
  }
}
