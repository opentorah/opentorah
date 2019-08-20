package org.podval.calendar.astronomy

import org.podval.calendar.angles.{Angles, Exactify, Interval}
import Angles.{Digit, Rotation}
import org.podval.calendar.jewish.Jewish
import org.podval.calendar.numbers.Digit

trait Time2Rotation {
  import Time2Rotation.Key

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

  final def calculate(vector: Jewish#Vector): Rotation = {
    val rational = vector.toRational
    calculate(rational.whole) + Rotation.fromRational(rational.fraction*one.toRational, 6)
  }

  protected def precision(key: Key): Digit = Digit.SECONDS

  final def calculateExact(days: Int): Rotation = rambamValue*days

  final def calculateExact(vector: Jewish#Vector): Rotation =
    Rotation.fromRational(vector.toRational*one.toRational, 6)

  final def exactify: Interval = {
    val exact = Seq(Key.Ten, Key.Hundred, Key.Thousand, Key.TenThousand) // Key.all
      .filterNot(value(_).isZero)
      .map(exactify)
      .reduce(_ intersect _)
    println(s"exact: $exact")
    exact
  }

  private final def exactify(key: Key): Interval = {
    val small = if (!one.isZero) one else ten
    val big = value(key)
    val mult = key.number
    val exactificator = new Exactify(small, if (!one.isZero) mult else mult/10, Angles.Digit.SECONDS.position, big)
    val (fit, fitLength) = exactificator.findFit
    val expanded = exactificator.expand(fit, fitLength, 6)
    println(s"$expanded (6)    $small * $mult -> $big: $fit ($fitLength)")
    expanded
  }
}

object Time2Rotation {
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
}
