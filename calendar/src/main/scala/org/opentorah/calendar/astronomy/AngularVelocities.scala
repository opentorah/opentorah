package org.opentorah.calendar.astronomy

import org.opentorah.calendar.angles.Angles
import org.opentorah.calendar.angles.Angles.Rotation
import org.opentorah.calendar.jewish.{Jewish, Sun}
import org.opentorah.numbers.BigRational

/*
 Al-Battani, WikiPedia:
 One of Al-Battani's best-known achievements in astronomy was the refinement of existing values
 for the length of the year.
 Ptolemy calculated the solar year values for the length of the year as being
 365 days, 5 hours, 55 minutes and 12 seconds.
 Al-Battani recalculated the solar year values for the length of the year as being
 365 days, 5 hours, 46 minutes and 24 seconds.
 He was able to correct some of Ptolemy's results and compiled new tables of the Sun and Moon,
 long accepted as authoritative. Al-Battānī rediscovered that the direction of the Sun's apogee,
 as recorded by Ptolemy, was changing. (In modern heliocentric terms this is due to the changing
 direction eccentricity vector of the Earth's orbit).
 He also elaborated to a specified degree a number of trigonometric relations, the use of sines in
 calculation, and partially that of tangents. He elaborated to a specified degree the work of an
 Indian astronomer Aryabhata(476–550 CE) and a Greek astronomer Pythagoras (570 BC – c. 495 BC).
 He also recalculated the values for the precession of the equinoxes (54.5" per year, or 1° in 66
 years) and the obliquity of the ecliptic (23° 35'), which was an elaboration of Hipparchus' work.
 */
object AngularVelocities {

  def main(args: Array[String]): Unit = {
//    SunLongitudeMean.exactify
    SunApogee.exactify

//    MoonAnomalyMean.exactify
    //    MoonLongitudeMean.exactify
    //    MoonHeadMean.exactify
  }

  def sunLongitudeMean(): Unit = {
    def m(n: Int): Rotation = (SunLongitudeMean.rambamValue * n).roundTo(Angles.Digit.SECONDS)
    for (n <- List(1, 10, 100, 1000, 10000, 354))
      println(n + " " + m(n))

    val v29 = Rotation(9, 51, 23)*3 - Rotation(0, 59, 8)
    println(v29)

    val v354 = Rotation(98, 33, 53)*3 + Rotation(9, 51, 23)*5 + Rotation(0, 59, 8)*4
    println(v354)

    def rat(rot: Rotation): BigRational = Rotation(360).toRational/rot.toRational
    def len(x: BigRational): Jewish.TimeVector = Jewish.TimeVector.fromRational(x, 3)

    val albatani = rat(SunLongitudeMean.albataniValue)
    val moznaim  = rat(SunLongitudeMean.rambamValue)
    val almagest = rat(SunLongitudeMean.almagestValue)

    println("Albatani: " + albatani.toDouble + " " + len(albatani))
    println("Moznaim : " + moznaim.toDouble + " " + len(moznaim))
    println("Almagest: " + almagest.toDouble + " " + len(almagest))
    println("Rav Ada : " + Sun.RavAda.yearLength.toRational.toDouble + " " + Sun.RavAda.yearLength)
    println("Shmuel  : " + Sun.Shmuel.yearLength.toRational.toDouble + " " + Sun.Shmuel.yearLength)

    println((SunLongitudeMean.almagestValue*1000).roundToMinutes)
  }
}
