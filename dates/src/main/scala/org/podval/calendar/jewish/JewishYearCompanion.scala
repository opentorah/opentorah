package org.podval.calendar.jewish

import org.podval.calendar.dates.YearCompanion
import Jewish.{Month, MonthNameAndLength, TimeInterval, YearCharacter}
import Month.Name._

abstract class JewishYearCompanion extends YearCompanion[Jewish] {
  final type Kind = JewishYearCompanion.Kind

  final val Kind: JewishYearCompanion.Kind.type = JewishYearCompanion.Kind

  protected final override def characters: Seq[YearCharacter] =
    for (isLeap <- Seq(true, false); kind <- Kind.values) yield (isLeap, kind)

  // KH 8:5-6
  protected final override def monthNamesAndLengths(character: YearCharacter):
    Seq[MonthNameAndLength] =
  {
    character match { case (isLeap: Boolean, kind: Kind) =>
      Seq(
        createMonthNameAndLength(Tishrei   , 30),
        createMonthNameAndLength(Marheshvan, if (kind == Kind.Full) 30 else 29),
        createMonthNameAndLength(Kislev    , if (kind == Kind.Short) 29 else 30),
        createMonthNameAndLength(Teves     , 29),
        createMonthNameAndLength(Shvat     , 30)
      ) ++
        (if (!isLeap)
          Seq(createMonthNameAndLength(Adar, 29))
        else
          Seq(createMonthNameAndLength(AdarI, 30), createMonthNameAndLength(AdarII, 29))) ++
        Seq(
          createMonthNameAndLength(Nisan , 30),
          createMonthNameAndLength(Iyar  , 29),
          createMonthNameAndLength(Sivan , 30),
          createMonthNameAndLength(Tammuz, 29),
          createMonthNameAndLength(Av    , 30),
          createMonthNameAndLength(Elul  , 29)
        )
    }
  }

  protected final override def areYearsPositive: Boolean = true

  final override def isLeap(yearNumber: Int): Boolean = Cycle.isLeapYear(yearNumber)

  final override def firstMonth(yearNumber: Int): Int = Cycle.firstMonth(yearNumber)

  final override def lengthInMonths(yearNumber: Int): Int = Cycle.yearLengthInMonths(yearNumber)

  // KH 8:7-8
  final def kind(isLeap: Boolean, lengthInDays: Int): Kind = {
    val daysOverShort: Int = lengthInDays - (if (isLeap) 383 else 353)

    daysOverShort match {
      case 0 => Kind.Short
      case 1 => Kind.Regular
      case 2 => Kind.Full
      case _ => throw new IllegalArgumentException(
        "Impossible year length " + lengthInDays + " for " + this)
    }
  }
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

  final val normalYear: TimeInterval = Moon.meanLunarPeriod*Cycle.yearLengthInMonths(isLeap = false)
  final val leapYear: TimeInterval = Moon.meanLunarPeriod*Cycle.yearLengthInMonths(isLeap = true)
}
