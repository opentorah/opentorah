package org.podval.calendar.jewish

import org.podval.calendar.dates.YearCompanion
import Jewish.{Day, Month, MonthNameAndLength, Moment, TimeInterval, YearCharacter}
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

  protected final override def areYearsPositive: Boolean = true

  final override def isLeap(yearNumber: Int): Boolean = Cycle.isLeapYear(yearNumber)

  final override def firstMonth(yearNumber: Int): Int = Cycle.firstMonth(yearNumber)

  final override def lengthInMonths(yearNumber: Int): Int = Cycle.yearLengthInMonths(yearNumber)
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

  final val normalYear: TimeInterval = JewishMonthCompanion.meanLunarPeriod*Cycle.yearLengthInMonths(isLeap = false)
  final val leapYear: TimeInterval = JewishMonthCompanion.meanLunarPeriod*Cycle.yearLengthInMonths(isLeap = true)

  // TODO package RoshHaShonoh calculations into a class

  // TODO KH 7:8 says that postponement of RoshHashonoh is done to align the calendar better with
  // the true molad; analyze the statistics of distances between mean *and* true molad and RoshHashono.
  // TODO are there meaningful names for the corrections?

  final def firstDayNumber(yearNumber: Int, newMoon: Moment): Int = {
    val correction =
      if (isAduCorrected(newMoon)) 1
      else if (isFirstCorrected(newMoon)) 1 + (if (isFirstAduCorrected(newMoon)) 1 /* KH 7:3 */ else 0 /* KH 7:2 */)
      else if (isSecondCorrected(yearNumber, newMoon)) 2  /* KH 7:4 */
      else if (isThirdCorrected(yearNumber, newMoon)) 1  /* KH 7:5 */
      else 0

    newMoon.day.number + correction
  }

  // KH 7:1
  final def isAduCorrected(newMoon: Moment): Boolean = isAdu(newMoon.day)
  private val adu: Set[Day.Name] = Set(Rishon, Rvii, Shishi)
  final def isAdu(day: Day): Boolean = adu.contains(day.name)

  // KH 7:1-2 (molad zoken)
  final def isFirstCorrected(newMoon: Moment): Boolean =
    !isAduCorrected(newMoon) && (newMoon.time >= firstCorrection)
  final val firstCorrection: TimeInterval  = TimeInterval().hours(18)

  // KH 7:3
  final def isFirstAduCorrected(newMoon: Moment): Boolean =
    isFirstCorrected(newMoon) && isAdu(newMoon.day.next)

  // KH 7:4
  final def isSecondCorrected(yearNumber: Int, newMoon: Moment): Boolean =
    !isAduCorrected(newMoon) &&
    !isFirstCorrected(newMoon) &&
    (newMoon.day.name == Day.Name.Shlishi) &&
    (newMoon.time >= secondCorrection) &&
    !Cycle.isLeapYear(yearNumber)
  final val secondCorrection: TimeInterval = TimeInterval().hours(9).parts(204)

  // KH 7:5
  // This is not defined for yer 0 - and doesn't apply :)
  final def isThirdCorrected(yearNumber: Int, newMoon: Moment): Boolean =
    !isAduCorrected(newMoon) &&
    !isFirstCorrected(newMoon) &&
    !isSecondCorrected(yearNumber, newMoon) &&
    (newMoon.day.name == Day.Name.Sheni) &&
    (newMoon.time >= thirdCorrection) &&
    Cycle.isLeapYear(yearNumber-1)
  // TODO this can be calculated based on the maximum length of a year and
  // the first correction; do it!
  final val thirdCorrection: TimeInterval  = TimeInterval().hours(15).parts(589)
}
