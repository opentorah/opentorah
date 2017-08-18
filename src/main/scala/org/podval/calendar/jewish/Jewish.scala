package org.podval.calendar.jewish

import org.podval.calendar.calendar._
import org.podval.calendar.numbers.NumberSystem.RawNumber

// TODO add a check that length of the year and total length of the months are the same
class Jewish private() extends Calendar[Jewish] {

  trait JewishCalendarMember extends CalendarMember[Jewish] {
    final override def calendar: Jewish = Jewish.this
  }

  abstract class JewishYear(number: Int) extends YearBase(number) { this: Year =>
    require(0 < number)

    // TODO the moment I change the type of newMoon from Moment to Jewish#Moment,
    // which I need to do to split this thing,
    // I get error for all three corrections:
    //   type mismatch;
    //   found   : Jewish.this.numberSystem.TimeInterval
    //   required: _1.numberSystem.TimeInterval where val _1: org.podval.calendar.jewish.Jewish
    // I think I need to derive Calendar from NumberSystem...
    final def newMoon: Moment = month(1).newMoon

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

    final override def character: YearCharacter = (isLeap, kind)

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

  final override def createYear(number: Int): Year =
    new JewishYear(number) with JewishCalendarMember

  final type YearKind = JewishYearKind

  // TODO stick this into the Year companion?
  final val YearKind: JewishYearKind.type = JewishYearKind

  final override type YearCharacter = (Boolean, YearKind)

  final override val Year: JewishYearCompanion =
    new JewishYearCompanion with JewishCalendarMember

  abstract class JewishYearCompanion extends YearCompanion[Jewish] {
    protected final override def characters: Seq[Jewish#YearCharacter] =
      for (isLeap <- Seq(true, false); kind <- YearKind.values) yield (isLeap, kind)

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
    final def normal: TimeInterval = Month.meanLunarPeriod*lengthInMonths(isLeap = false)

    final def leap: TimeInterval = Month.meanLunarPeriod*lengthInMonths(isLeap = true)

    final def lengthInMonths(isLeap: Boolean): Int = if (isLeap) 13 else 12

    final val yearsInCycle: Int = 19

    final val leapYearsInCycle: Int = leapYears.size

    final val monthsBeforeYearInCycle: Seq[Int] =
      ((1 to yearsInCycle) map lengthInMonths).scanLeft(0)(_ + _)

    final val monthsInCycle: Int = monthsBeforeYearInCycle.last

    final def cycleLength: TimeInterval = Month.meanLunarPeriod * monthsInCycle

    final def firstMonthInCycle(yearNumber: Int): Int =
      monthsBeforeYearInCycle(numberInCycle(yearNumber) - 1) + 1

    final def numberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1

    final def cycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1

    // TODO meaningful names
    final val firstCorrection  = interval.hours(18) // KH 7:1
    final val secondCorrection = interval.hours(9).parts(204) // KH 7:4
    final val thirdCorrection  = interval.hours(15).parts(589) // KH 7:5
  }

  final override type Month = JewishMonth

  final override def createMonth(number: Int): Month = new JewishMonth(number) with JewishCalendarMember

  abstract class JewishMonth(number: Int) extends MonthBase[Jewish](number) { this: Jewish#Month =>
    // TODO before I can split this out, I need to change return type of the next method to C#Moment;
    // that leads to compilation errors for the 3 corrections - and so do the attempts to
    // split out Calendar.Year etc...
    final def newMoon: Moment = Month.firstNewMoon + Month.meanLunarPeriod*(number-1)
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

  final override def createDay(number: Int): Day = new JewishDay(number) with JewishCalendarMember

  final override type DayName = JewishDayName

  // TODO stick it into the Day companion???
  val DayName: JewishDayName.type = JewishDayName

  final override val Day: JewishDayCompanion = new JewishDayCompanion with JewishCalendarMember

  abstract class JewishMoment(raw: RawNumber) extends MomentBase(raw) {
    final def nightHours(value: Int): Moment = firstHalfHours(value)

    final def dayHours(value: Int): Moment = secondHalfHours(value)
  }

  final override type Moment = JewishMoment

  final override def createMoment(raw: RawNumber): Moment =
    new JewishMoment(raw) with JewishCalendarMember

  final override val Moment: JewishMomentCompanion =
    new JewishMomentCompanion with JewishCalendarMember
}


object Jewish extends Jewish
