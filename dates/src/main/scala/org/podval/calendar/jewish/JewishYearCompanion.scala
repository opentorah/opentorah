package org.podval.calendar.jewish

import org.podval.calendar.dates.YearCompanion
import Jewish.{Day, Month, MonthNameAndLength, TimeInterval, YearCharacter, interval}
import Day.Name._
import Month.Name._

abstract class JewishYearCompanion extends YearCompanion[Jewish] {
  final type Kind = JewishYearCompanion.Kind

  final val Kind: JewishYearCompanion.Kind.type = JewishYearCompanion.Kind

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

  // KH 7:1
  private val adu: Set[Day.Name] = Set(Rishon, Rvii, Shishi)
  final def isAdu(day: Day): Boolean = adu.contains(day.name)

  protected final override def areYearsPositive: Boolean = true

  final val leapYears: Set[Int] =
    Set(3, 6, 8, 11, 14, 17, 19) // TODO calculate Meton's cycle in the paper

  final override def isLeap(yearNumber: Int): Boolean =
    leapYears.contains(numberInCycle(yearNumber))

  final override def firstMonth(yearNumber: Int): Int =
    monthsInCycle*(cycle(yearNumber) - 1) + firstMonthInCycle(yearNumber)

  final override def lengthInMonths(yearNumber: Int): Int = lengthInMonths(isLeap(yearNumber))

  final val normal: TimeInterval = Month.meanLunarPeriod*lengthInMonths(isLeap = false)

  final val leap: TimeInterval = Month.meanLunarPeriod*lengthInMonths(isLeap = true)

  final def lengthInMonths(isLeap: Boolean): Int = if (isLeap) 13 else 12

  // TODO move into Cycle?

  final val yearsInCycle: Int = 19

  final val leapYearsInCycle: Int = leapYears.size

  final val monthsBeforeYearInCycle: Seq[Int] =
    ((1 to yearsInCycle) map lengthInMonths).scanLeft(0)(_ + _)

  final val monthsInCycle: Int = monthsBeforeYearInCycle.last

  final val cycleLength: TimeInterval = Month.meanLunarPeriod * monthsInCycle

  final def firstMonthInCycle(yearNumber: Int): Int =
    monthsBeforeYearInCycle(numberInCycle(yearNumber) - 1) + 1

  final def numberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1

  final def cycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1

  // TODO package RoshHaShonoh calculations into a class
  // TODO are there meaningful names for these things?
  final val firstCorrection: TimeInterval  = interval.hours(18) // KH 7:1
  final val secondCorrection: TimeInterval = interval.hours(9).parts(204) // KH 7:4

  // KH 7:5
  // TODO this can be calculated based on the maximum length of a year and
  // the first correction; do it!
  final val thirdCorrection: TimeInterval  = interval.hours(15).parts(589)
}


object JewishYearCompanion {
  // KH 8:6
  sealed trait Kind
  object Kind {
    case object Short extends Kind
    case object Regular extends Kind
    case object Full extends Kind

    val values: Seq[Kind] = Seq(Short, Regular, Full)
  }
}
