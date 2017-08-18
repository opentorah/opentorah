package org.podval.calendar.jewish

import org.podval.calendar.calendar.YearCompanion

abstract class JewishYearCompanion extends YearCompanion[Jewish] {
  protected final override def characters: Seq[Jewish#YearCharacter] =
    for (isLeap <- Seq(true, false); kind <- calendar.YearKind.values) yield (isLeap, kind)

  // KH 8:5-6
  protected final override def monthNamesAndLengths(character: Jewish#YearCharacter):
  List[Jewish#MonthNameAndLength] =
  {
    def create(name: Jewish#MonthName, length: Int): Jewish#MonthNameAndLength =
      calendar.createMonthNameAndLength(name, length)

    import Jewish.MonthName._
    character match { case (isLeap: Boolean, kind: Jewish#YearKind) =>
      List(
        create(Tishrei   , 30),
        create(Marheshvan, if (kind == Jewish.YearKind.Full) 30 else 29),
        create(Kislev    , if (kind == Jewish.YearKind.Short) 29 else 30),
        create(Teves     , 29),
        create(Shvat     , 30)
      ) ++
        (if (!isLeap)
          List(create(Adar, 29))
        else
          List(create(AdarI, 30), create(AdarII, 29))) ++
        List(
          create(Nisan , 30),
          create(Iyar  , 29),
          create(Sivan , 30),
          create(Tammuz, 29),
          create(Av    , 30),
          create(Elul  , 29)
        )
    }
  }

  private val adu: Set[Jewish#DayName] =
    Set(Jewish.DayName.Rishon, Jewish.DayName.Rvii, Jewish.DayName.Shishi)

  final def isAdu(day: Jewish#Day): Boolean = adu.contains(day.name)

  protected final override def areYearsPositive: Boolean = true

  private[this] val leapYears: Set[Int] =
    Set(3, 6, 8, 11, 14, 17, 19) // TODO calculate Meton's cycle in the paper

  final override def isLeap(yearNumber: Int): Boolean =
    leapYears.contains(numberInCycle(yearNumber))

  final override def firstMonth(yearNumber: Int): Int =
    monthsInCycle*(cycle(yearNumber) - 1) + firstMonthInCycle(yearNumber)

  final override def lengthInMonths(yearNumber: Int): Int = lengthInMonths(isLeap(yearNumber))

  // TODO parameterless defs aren't vals so that initialization works :)
  final def normal: Jewish#Interval =
    calendar.Month.meanLunarPeriod*lengthInMonths(isLeap = false)

  final def leap: Jewish#Interval =
    calendar.Month.meanLunarPeriod*lengthInMonths(isLeap = true)

  final def lengthInMonths(isLeap: Boolean): Int = if (isLeap) 13 else 12

  final val yearsInCycle: Int = 19

  final val leapYearsInCycle: Int = leapYears.size

  final val monthsBeforeYearInCycle: Seq[Int] =
    ((1 to yearsInCycle) map lengthInMonths).scanLeft(0)(_ + _)

  final val monthsInCycle: Int = monthsBeforeYearInCycle.last

  final def cycleLength: Jewish#Interval = calendar.Month.meanLunarPeriod * monthsInCycle

  final def firstMonthInCycle(yearNumber: Int): Int =
    monthsBeforeYearInCycle(numberInCycle(yearNumber) - 1) + 1

  final def numberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1

  final def cycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1

  // TODO meaningful names
  final val firstCorrection  = calendar.interval.hours(18) // KH 7:1
  final val secondCorrection = calendar.interval.hours(9).parts(204) // KH 7:4
  final val thirdCorrection  = calendar.interval.hours(15).parts(589) // KH 7:5
}
