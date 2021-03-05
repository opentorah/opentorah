package org.opentorah.astronomy

import org.opentorah.angles.{Angles, Exactify, Interval}
import org.opentorah.calendar.jewish.Jewish
import org.opentorah.numbers.Digit
import Angles.{Digit, Rotation}

object Time2Rotation {
  type Days = Int
}

trait Time2Rotation {
  import Time2Rotation.Days

  final def value(days: Days): Rotation = days match {
    case     1 => one
    case    10 => ten
    case   100 => hundred
    case  1000 => thousand
    case 10000 => tenThousand
    case    29 => month
    case   354 => year
  }

  def one        : Rotation
  def ten        : Rotation
  def hundred    : Rotation
  def thousand   : Rotation
  def tenThousand: Rotation

  def month      : Rotation
  def year       : Rotation

  val almagestValue: Rotation

  val rambamValue: Rotation

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
      (if (lessThanHundred == 29) month else ten*tens + one*ones)
  }

  final def calculate(vector: Jewish.Vector): Rotation = {
    val rational = vector.toRational
    calculate(rational.whole) + Rotation.fromRational(rational.fraction*one.toRational, 6)
  }

  protected def precision(days: Days): Digit = Digit.SECONDS

  final def calculateExact(days: Int): Rotation = rambamValue*days

  final def calculateExact(vector: Jewish.Vector): Rotation =
    Rotation.fromRational(vector.toRational*one.toRational, 6)

  final def exactify: Interval = {
    val exact = Seq(10, 100, 1000, 10000) // all?
      .filterNot(value(_).isZero)
      .map(exactify)
      .reduce(_ intersect _)
    println(s"exact: $exact")
    exact
  }

  private final def exactify(days: Days): Interval = {
    val small = if (!one.isZero) one else ten
    val big = value(days)
    val mult = days
    val exactificator = new Exactify(small, if (!one.isZero) mult else mult/10, Angles.Digit.SECONDS.position, big)
    val (fit, fitLength) = exactificator.findFit
    val expanded = exactificator.expand(fit, fitLength, 6)
    println(s"$expanded (6)    $small * $mult -> $big: $fit ($fitLength)")
    expanded
  }
}

