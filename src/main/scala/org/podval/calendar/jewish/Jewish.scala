package org.podval.calendar.jewish

import org.podval.calendar.calendar._
import org.podval.calendar.numbers.NumberSystem.RawNumber

// TODO add a check that length of the year and total length of the months are the same
class Jewish private() extends Calendar[Jewish] {

  trait JewishCalendarMember extends CalendarMember[Jewish] {
    final override def calendar: Jewish = Jewish.this
  }

  abstract class JewishYear(number: Int) extends YearBase(number) { this: Jewish#Year =>
    require(0 < number)

    // TODO the moment I change the type of newMoon from Moment to Jewish#Moment,
    // which I need to do to split this thing,
    // I get error for all three corrections:
    //   type mismatch;
    //   found   : Jewish.this.numberSystem.TimeInterval
    //   required: _1.numberSystem.TimeInterval where val _1: org.podval.calendar.jewish.Jewish
    // I think I need to derive Calendar from NumberSystem...
    final def newMoon: Jewish#Moment = month(1).newMoon

    final override def firstDayNumber: Int = {
      val correction =
        if (isAduCorrected) 1
        else if (isFirstCorrected) 1 + (if (isFirstAduCorrected) 1 /* KH 7:3 */ else 0 /* KH 7:2 */)
        else if (isSecondCorrected) 2  /* KH 7:4 */
        else if (isThirdCorrected ) 1  /* KH 7:5 */
        else 0

      newMoon.day.number + correction
    }

    final def isAduCorrected: Boolean = Year.isAdu(newMoon.day)  // KH 7:1

    final def isFirstCorrected: Boolean = !isAduCorrected && (newMoon.time >= Year.firstCorrection)

    final def isFirstAduCorrected: Boolean = isFirstCorrected && Year.isAdu(newMoon.day.next)

    final def isSecondCorrected: Boolean = !isAduCorrected && !isFirstCorrected &&
      (newMoon.day.name == DayName.Shlishi) && (newMoon.time >= Year.secondCorrection) &&
      !this.isLeap

    // This is not defined for yer 0 - and doesn't apply :)
    final def isThirdCorrected: Boolean = !isAduCorrected && !isFirstCorrected &&
      !isSecondCorrected &&
      (newMoon.day.name == DayName.Sheni) && (newMoon.time >= Year.thirdCorrection) &&
      this.prev.isLeap

    final override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

    final def cycle: Int = Year.cycle(number)

    final def numberInCycle: Int = Year.numberInCycle(number)

    final override def character: Jewish#YearCharacter = (isLeap, kind)

    // KH 8:7,8
    final def kind: YearKind = {
      val daysOverShort = lengthInDays - (if (isLeap) 383 else 353)

      daysOverShort match {
        case 0 => YearKind.Short
        case 1 => YearKind.Regular
        case 2 => YearKind.Full
        case _ => throw new IllegalArgumentException(
          "Impossible year length " + lengthInDays + " for " + this)
      }
    }
  }

  final override type Year = JewishYear

  final override def createYear(number: Int): Jewish#Year =
    new JewishYear(number) with JewishCalendarMember

  final type YearKind = JewishYearKind

  // TODO stick this into the Year companion?
  final val YearKind: JewishYearKind.type = JewishYearKind

  final override type YearCharacter = (Boolean, YearKind)

  final override val Year: JewishYearCompanion =
    new JewishYearCompanion with JewishCalendarMember


  final override type Month = JewishMonth

  final override def createMonth(number: Int): Jewish#Month =
    new JewishMonth(number) with JewishCalendarMember

  abstract class JewishMonth(number: Int) extends MonthBase[Jewish](number) { this: Jewish#Month =>
    // TODO before I can split this out, I need to change return type of the next method to C#Moment;
    // that leads to compilation errors for the 3 corrections - and so do the attempts to
    // split out Calendar.Year etc...
    final def newMoon: Jewish#Moment = Month.firstNewMoon + Month.meanLunarPeriod*(number-1)
  }


  final override type MonthName = JewishMonthName

  // TODO stick it into the Month companion???
  final val MonthName: JewishMonthName.type = JewishMonthName

  abstract class JewishMonthCompanion extends MonthCompanion {
    // KH 6:3
    // TODO how is this really called? tropical?
    final val meanLunarPeriod = interval.days(29).hours(12).parts(793)

    // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2):
    // BeHaRaD: (KH 6:8)
    final val firstNewMoon = moment.day(2).nightHours(5).parts(204)

    final override def yearNumber(monthNumber: Int): Int = {
      val cycleOfMonth = ((monthNumber - 1) / Year.monthsInCycle) + 1
      val yearsBeforeCycle = (cycleOfMonth - 1) * Year.yearsInCycle
      val yearMonthIsInCycle =
        Year.monthsBeforeYearInCycle.count(_ < numberInCycleOfMonth(monthNumber))
      yearsBeforeCycle + yearMonthIsInCycle
    }

    final override def numberInYear(monthNumber: Int): Int =
      numberInCycleOfMonth(monthNumber) - Year.firstMonthInCycle(yearNumber(monthNumber)) + 1

    private def numberInCycleOfMonth(monthNumber: Int): Int =
      ((monthNumber - 1) % Year.monthsInCycle) + 1
  }

  final override val Month: JewishMonthCompanion =
    new JewishMonthCompanion with JewishCalendarMember

  final override type Day = JewishDay

  final override def createDay(number: Int): Jewish#Day =
    new JewishDay(number) with JewishCalendarMember

  final override type DayName = JewishDayName

  // TODO stick it into the Day companion???
  val DayName: JewishDayName.type = JewishDayName

  final override val Day: JewishDayCompanion = new JewishDayCompanion with JewishCalendarMember

  abstract class JewishMoment(raw: RawNumber) extends MomentBase[Jewish](raw) {
    final def nightHours(value: Int): Jewish#Moment = firstHalfHours(value)

    final def dayHours(value: Int): Jewish#Moment = secondHalfHours(value)
  }

  final override type Moment = JewishMoment

  final override def createMoment(raw: RawNumber): Jewish#Moment =
    new JewishMoment(raw) with JewishCalendarMember {
      final override def numberSystem:  Jewish = Jewish.this
    }

  final override val Moment: JewishMomentCompanion =
    new JewishMomentCompanion with JewishCalendarMember
}


object Jewish extends Jewish
