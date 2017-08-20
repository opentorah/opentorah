package org.podval.calendar.jewish

import org.podval.calendar.calendar.YearCompanion
import Jewish._ // TODO {Year, Month, Day, Interval, interval, YearCharacter, MonthNameAndLength}
import Day.Name._
import Month.Name._

abstract class JewishYearCompanion extends YearCompanion[Jewish] {
  final type Kind = JewishYearKind

  final val Kind: JewishYearKind.type = JewishYearKind

  protected final override def characters: Seq[YearCharacter] =
    for (isLeap <- Seq(true, false); kind <- Kind.values) yield (isLeap, kind)

  // KH 8:5-6
  protected final override def monthNamesAndLengths(character: YearCharacter):
  List[MonthNameAndLength] =
  {
    character match { case (isLeap: Boolean, kind: Kind) =>
      List(
        createMonthNameAndLength(Tishrei   , 30),
        createMonthNameAndLength(Marheshvan, if (kind == Kind.Full) 30 else 29),
        createMonthNameAndLength(Kislev    , if (kind == Kind.Short) 29 else 30),
        createMonthNameAndLength(Teves     , 29),
        createMonthNameAndLength(Shvat     , 30)
      ) ++
        (if (!isLeap)
          List(createMonthNameAndLength(Adar, 29))
        else
          List(createMonthNameAndLength(AdarI, 30), createMonthNameAndLength(AdarII, 29))) ++
        List(
          createMonthNameAndLength(Nisan , 30),
          createMonthNameAndLength(Iyar  , 29),
          createMonthNameAndLength(Sivan , 30),
          createMonthNameAndLength(Tammuz, 29),
          createMonthNameAndLength(Av    , 30),
          createMonthNameAndLength(Elul  , 29)
        )
    }
  }

  private val adu: Set[Day.Name] = Set(Rishon, Rvii, Shishi)

  final def isAdu(day: Day): Boolean = adu.contains(day.name)

  protected final override def areYearsPositive: Boolean = true

  private[this] val leapYears: Set[Int] =
    Set(3, 6, 8, 11, 14, 17, 19) // TODO calculate Meton's cycle in the paper

  final override def isLeap(yearNumber: Int): Boolean =
    leapYears.contains(numberInCycle(yearNumber))

  final override def firstMonth(yearNumber: Int): Int =
    monthsInCycle*(cycle(yearNumber) - 1) + firstMonthInCycle(yearNumber)

  final override def lengthInMonths(yearNumber: Int): Int = lengthInMonths(isLeap(yearNumber))

  // TODO parameterless defs aren't vals so that initialization works :)
  final def normal: Interval = Month.meanLunarPeriod*lengthInMonths(isLeap = false)

  final def leap: Interval = Month.meanLunarPeriod*lengthInMonths(isLeap = true)

  final def lengthInMonths(isLeap: Boolean): Int = if (isLeap) 13 else 12

  final val yearsInCycle: Int = 19

  final val leapYearsInCycle: Int = leapYears.size

  final val monthsBeforeYearInCycle: Seq[Int] =
    ((1 to yearsInCycle) map lengthInMonths).scanLeft(0)(_ + _)

  final val monthsInCycle: Int = monthsBeforeYearInCycle.last

  final def cycleLength: Interval = Month.meanLunarPeriod * monthsInCycle

  final def firstMonthInCycle(yearNumber: Int): Int =
    monthsBeforeYearInCycle(numberInCycle(yearNumber) - 1) + 1

  final def numberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1

  final def cycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1

  // TODO meaningful names
  final val firstCorrection: Interval  = interval.hours(18) // KH 7:1
  final val secondCorrection: Interval = interval.hours(9).parts(204) // KH 7:4
  final val thirdCorrection: Interval  = interval.hours(15).parts(589) // KH 7:5
}
