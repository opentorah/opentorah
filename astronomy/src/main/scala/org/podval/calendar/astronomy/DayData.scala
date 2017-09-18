package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem
import AngleNumberSystem.Angle


/*
 TODO
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

import DayData.{Days, Table}

trait DayData {

  val table: Table

  final def fromTable(table: Table)(days: Int): Angle = {
    val tenThousands: Int =  days          / 10000
    val thousands   : Int = (days % 10000) /  1000
    val hundreds    : Int = (days %  1000) /   100
    val tens        : Int = (days %   100) /    10
    val ones        : Int =  days %    10

    table.tenThousand*tenThousands +
    table.thousand   *thousands +
    table.hundred    *hundreds +
    table.ten        *tens +
    table.one        *ones
  }

  final def fromTable(days: Int): Angle = fromTable(table)(days)

  val rambamValue: Angle

  val almagestValue: Angle

  final def fromValue(value: Angle)(days: Days): Angle = value*days

  final def fromValue(days: Days): Angle = fromValue(rambamValue)(days)



  final def exact: Angle = reconstructed(10000)

  final def exact10: Double = reconstructed10(10000)

  final def reconstructed(days: Days): Angle =
    Angle.fromDegrees(reconstructed10(days), 6) // 6 60-digits

  final def reconstructed10(days: Days): Double = exactify(table.one, days, fromTable(days))

  final def recalculated(days: Days): Angle = exact*days

  final def recalculated10(days: Days): Angle = Angle.fromDegrees(exact10*days, 6)

  final def almagest(days: Days): Angle = almagestValue*days

  final def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
    val fullRotations = math.floor(days*approximate.toDouble/AngleNumberSystem.headRange.toDouble).toInt
    (AngleNumberSystem.headRange.toDouble*fullRotations + angle.toDegrees)/days
  }
}


object DayData {
  type Days = Int

  trait Table {
    def one        : Angle //     1
    def ten        : Angle //    10
    def hundred    : Angle //   100
    def thousand   : Angle //  1000
    def tenThousand: Angle // 10000

    def month      : Angle //    29
    def year       : Angle //   354
  }

  final val keys: Seq[Days] = Seq(10, 100, 1000, 10000, 29, 254)
}
