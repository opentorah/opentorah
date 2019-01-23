package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles
import Angles.Rotation
import org.podval.calendar.jewish.Jewish

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

trait Days2Angle {
  import Days2Angle.Key

  def one        : Rotation
  def ten        : Rotation
  def hundred    : Rotation
  def thousand   : Rotation
  def tenThousand: Rotation

  def month      : Rotation
  def year       : Rotation

  final def value(key: Key): Rotation = key match {
    case Key.One         => one
    case Key.Ten         => ten
    case Key.Hundred     => hundred
    case Key.Thousand    => thousand
    case Key.TenThousand => tenThousand
    case Key.Month       => month
    case Key.Year        => year
  }

  val almagestValue: Rotation

  val rambamValue: Rotation

  // TODO see if variations in this algorithms are logical: e.g., for 600, add for 1000 and subtract 4*for 100?
  // TODO see if the end result is stable when Rambam's "real" value is used with straight multiplication and rounding
  //   (abstract away the calculation mechaninsm).
  final def calculate(days: Int): Rotation = {
    val tenThousands: Int =  days          / 10000
    val thousands   : Int = (days % 10000) /  1000
    val hundreds    : Int = (days %  1000) /   100
    val lessThanHundred: Int = days % 100
    val tens        : Int = (days %   100) /    10
    val ones        : Int =  days %    10

    tenThousand*tenThousands +
      thousand*thousands +
      hundred*hundreds +
      // TODO without the '29' case, mean sun longitude for 4938/Iyar/2 is not what Rambam quotes in KH 15:8-9 (see test).
      (if (lessThanHundred == 29) month else ten*tens + one*ones)
  }

  final def calculate(vector: Jewish#Vector): Rotation = {
    val rational = vector.toRational
    calculate(rational.whole) + Rotation.fromRational(rational.fraction*one.toRational, 6)
  }

  final def calculateExact(days: Int): Rotation = rambamValue*days

  final def calculateExact(vector: Jewish#Vector): Rotation =
    Rotation.fromRational(vector.toRational*one.toRational, 6)

  final def exactify: Interval = {
    val exact = Seq(Key.Ten, Key.Hundred, Key.Thousand, Key.TenThousand) // Key.all
      .map(exactify).reduce(_ intersect _)
    println(s"exact: $exact")
    exact
  }

  final def exactify(key: Key): Interval = {
    val small = one
    val big = value(key)
    val mult = key.number
    val exactificator = new Exactify(small, mult, Angles.Digit.SECONDS.position, big)
    val (fit, fitLength) = exactificator.findFit
    val expanded = exactificator.expand(fit, fitLength, 6)
    println(s"$expanded (6)    $small * $mult -> $big: $fit ($fitLength)")
    expanded
  }
}

object Days2Angle {
  type Days = Int

  sealed abstract class Key(val number: Int)

  object Key {
    case object One         extends Key(    1)
    case object Ten         extends Key(   10)
    case object Hundred     extends Key(  100)
    case object Thousand    extends Key( 1000)
    case object TenThousand extends Key(10000)
    case object Month       extends Key(   29)
    case object Year        extends Key(  354)

    val values: Seq[Key] = Seq(One, Ten, Hundred, Thousand, TenThousand, Month, Year)
  }

  def main(args: Array[String]): Unit = {
    MoonAnomalyMean.exactify
    //    MoonLongitudeMean.exactify
    //    MoonHeadMean.exactify
    //    SunLongitudeMean.exactify
    //    SunApogee.exactify // TODO doesn't work!
  }
}
