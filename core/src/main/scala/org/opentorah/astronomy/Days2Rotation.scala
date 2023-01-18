package org.opentorah.astronomy

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.numbers.Exactify
import Angles.{Digit, Rotation}
import Rotation.Interval

object Days2Rotation:
  enum Days(val number: Int) derives CanEqual:
    case One extends Days(1)
    case Ten extends Days(10)
    case Hundred extends Days(100)
    case Thousand extends Days(1000)
    case TenThousand extends Days(10000)
    case Month extends Days(29)
    case Year extends Days(354)

  object Days:
    val all: Seq[Days] = Seq(One, Ten, Month, Hundred, Year, Thousand, TenThousand)
    val reconstructFrom: Seq[Days] = Seq(One, Ten, Hundred, Thousand)

  // multiplier to reconstruct from the key each of the values
  private def reconstruct: Map[Days, Map[Days, Option[Int]]] = Map(
    Days.One -> Map(
      Days.Ten         -> Some(10),
      Days.Month       -> Some(Days.Month.number),
      Days.Hundred     -> Some(100),
      Days.Year        -> Some(Days.Year.number),
      Days.Thousand    -> Some(1000),
      Days.TenThousand -> Some(10000)
    ),
    Days.Ten -> Map(
      Days.Ten         -> None,
      Days.Month       -> None,
      Days.Hundred     -> Some(10),
      Days.Year        -> None,
      Days.Thousand    -> Some(100),
      Days.TenThousand -> Some(1000)
    ),
    Days.Hundred -> Map(
      Days.Ten         -> None,
      Days.Month       -> None,
      Days.Hundred     -> None,
      Days.Year        -> None,
      Days.Thousand    -> Some(10),
      Days.TenThousand -> Some(100)
    ),
    Days.Thousand -> Map(
      Days.Ten         -> None,
      Days.Month       -> None,
      Days.Hundred     -> None,
      Days.Year        -> None,
      Days.Thousand    -> None,
      Days.TenThousand -> Some(10)
    )
  )

abstract class Days2Rotation(val name: String, values: (Days2Rotation.Days, String)*)
  extends MapTable[Days2Rotation.Days, Rotation](values*)(Rotation(_)):
  import Days2Rotation.Days

  private def isOneDefined: Boolean = !value(Days.One).isZero

  final def all: Seq[Days] = if isOneDefined then Days.all else Days.all.tail
  final def reconstructFrom: Seq[Days] = if isOneDefined then Days.reconstructFrom else Days.reconstructFrom.tail
  final def exactify: Seq[Days] = if isOneDefined then Days.all else Seq(Days.Ten, Days.Hundred, Days.Thousand, Days.TenThousand)

  val almagestValue: Rotation

  val rambamValue: Rotation

  final def calculate(days: Int): Rotation =
    val tenThousands: Int =  days          / 10000
    val thousands   : Int = (days % 10000) /  1000
    val hundreds    : Int = (days %  1000) /   100
    val lessThanHundred: Int = days % 100
    val tens        : Int = (days %   100) /    10
    val ones        : Int =  days %    10

    value(Days.TenThousand)*tenThousands +
    value(Days.Thousand)*thousands +
    value(Days.Hundred)*hundreds +
    (if lessThanHundred == 29 then value(Days.Month) else value(Days.Ten)*tens + value(Days.One)*ones)

  final def reconstructed: Map[Days, Map[Days, Option[Int]]] = (
    for from: Days <- reconstructFrom yield from -> {
      for (to: Days, multiplier: Option[Int]) <- Days2Rotation.reconstruct(from) yield to -> multiplier.map { (multiplier: Int) =>
        val reconstructedValue: Rotation = (value(from) * multiplier).canonical.roundTo(precision(to).position)
        val signum: Int = (reconstructedValue - value(to)).signum
        signum
      }
    }).toMap

  final def nonReconstructable: Iterable[(Days, Days, Option[Int])] =
    for
      (from: Days, reconstructed: Map[Days, Option[Int]]) <- reconstructed
      (to: Days, signum: Option[Int]) <- reconstructed
      if signum.isDefined && !signum.contains(0)
    yield
      (from, to, signum)

  private def exactificator(days: Days) = Exactify(Angles)(
    small = value(exactify.head),
    multiplier = days.number / exactify.head.number,
    round = precision(days),
    big = value(days)
  )

  final def exactified(maxLength: Int): Map[Days, (Int, Interval, Interval)] = (
    for days <- exactify yield days -> {
      val ex: Exactify = exactificator(days)
      val first = ex.find(maxLength)
      val precise = ex.findPrecise(maxLength)
      val (minInterval: Interval, min: Int) = first.get.asInstanceOf[(Interval, Int)] // TODO yuck
      val maxInterval: Interval = precise.get.asInstanceOf[Interval] // TODO yuck
      (min, minInterval, maxInterval)
    }).toMap

  def findPrecise(days: Days, length: Int): Option[Angles.Vector.Interval] =
    exactificator(days).findPrecise(length).asInstanceOf[Option[Angles.Vector.Interval]] // TODO yuck

//  final def calculate(vector: Jewish.Vector): Rotation =
//    val rational = vector.toRational
//    calculate(rational.whole) + Rotation.fromRational(rational.fraction*value(Days.One).toRational, 6)

  protected def precision(days: Days): Angles.Digit = Digit.SECONDS

//  final def calculateExact(days: Int): Rotation = rambamValue*days
//
//  final def calculateExact(vector: Jewish.Vector): Rotation =
//    Rotation.fromRational(vector.toRational*value(Days.One).toRational, 6)

//  final def exactify: Interval =
//    val exact = Seq(10, 100, 1000, 10000) // all?
//      .filterNot(value(_).isZero)
//      .map(exactify)
//      .reduce(_.intersect(_))
//    println(s"exact: $exact")
//    exact
//
//  private final def exactify(days: Days): Interval =
//    val small = if !one.isZero then one else ten
//    val big = value(days)
//    val mult = days
//    val exactificator = Exactify(Angles)(
//      small,
//      if !one.isZero then mult else mult/10,
//      Angles.Digit.SECONDS.position,
//      big
//    )
//    val (fit, fitLength) = exactificator.findFit
//    val expanded = exactificator.expand(fit, fitLength, 6)
//    println(s"$expanded (6)    $small * $mult -> $big: $fit ($fitLength)")
//    expanded.asInstanceOf[Angles.Rotation.Interval] // TODO yuck...
//
