package org.podval.calendar.gregorian

import org.podval.calendar.calendar._
import org.podval.calendar.jewish.Jewish
import org.podval.calendar.util.Named

class Gregorian private() extends Calendar[Gregorian] {

  trait GregorianCalendarMember extends CalendarMember[Gregorian] {
    final override def calendar: Gregorian = Gregorian.this
  }

  final class Year(number: Int)
    extends YearBase(number) with GregorianCalendarMember
  {
    override def firstDayNumber: Int = Year.firstDay(number)

    override def lengthInDays: Int = Year.lengthInDays(number)

    override def character: YearCharacter = isLeap
  }


  override type YearCharacter = Boolean

  object Year extends YearCompanion {
    override def apply(number: Int): Year = new Year(number)

    protected override def characters: Seq[YearCharacter] = Seq(true, false)

    protected override def monthNamesAndLengths(isLeap: YearCharacter): List[MonthNameAndLength] = {
      import MonthName._
      List(
        MonthNameAndLength(January  , 31),
        MonthNameAndLength(February , if (isLeap) 29 else 28),
        MonthNameAndLength(March    , 31),
        MonthNameAndLength(April    , 30),
        MonthNameAndLength(May      , 31),
        MonthNameAndLength(June     , 30),
        MonthNameAndLength(July     , 31),
        MonthNameAndLength(August   , 31),
        MonthNameAndLength(September, 30),
        MonthNameAndLength(October  , 31),
        MonthNameAndLength(November , 30),
        MonthNameAndLength(December , 31)
      )
    }

    protected override def areYearsPositive: Boolean = false

    override def isLeap(yearNumber: Int): Boolean = (yearNumber % 4 == 0) && ((yearNumber % 100 != 0) || (yearNumber % 400 == 0))

    override def firstMonth(yearNumber: Int): Int = monthsInYear*(yearNumber - 1) + 1

    override def lengthInMonths(yearNumber: Int): Int = monthsInYear

    val monthsInYear: Int = 12

    private val daysInNonLeapYear: Int = 365

    def firstDay(yearNumber: Int): Int = daysInNonLeapYear * (yearNumber - 1) + (yearNumber - 1)/4 - (yearNumber - 1)/100 + (yearNumber - 1)/400 + 1

    def lengthInDays(yearNumber: Int): Int = if (Gregorian.Year.isLeap(yearNumber)) daysInNonLeapYear + 1 else daysInNonLeapYear
  }


  final class Month(number: Int)
    extends MonthBase[Gregorian](number) with GregorianCalendarMember

  sealed class MonthName(name: String) extends Named(name)

  object MonthName {
    case object January extends MonthName("January")
    case object February extends MonthName("February")
    case object March extends MonthName("March")
    case object April extends MonthName("April")
    case object May extends MonthName("May")
    case object June extends MonthName("June")
    case object July extends MonthName("July")
    case object August extends MonthName("August")
    case object September extends MonthName("September")
    case object October extends MonthName("October")
    case object November extends MonthName("November")
    case object December extends MonthName("December")
  }

  object Month extends MonthCompanion {
    override def apply(number: Int): Month = new Month(number)

    override def yearNumber(monthNumber: Int): Int = (monthNumber - 1) / Gregorian.Year.monthsInYear + 1

    override def numberInYear(monthNumber: Int): Int =  monthNumber - Gregorian.Year.firstMonth(yearNumber(monthNumber)) + 1
  }


  override type Day = GregorianDay

  final def createDay(number: Int): Day = new GregorianDay(number) with GregorianCalendarMember

  abstract class GregorianDay(number: Int) extends DayBase[Gregorian](number) {
    this: Day =>
  }

  sealed class DayName(name: String) extends Named(name)

  object DayName {
    case object Sunday extends DayName("Sunday")
    case object Monday extends DayName("Monday")
    case object Tuesday extends DayName("Tuesday")
    case object Wednesday extends DayName("Wednesday")
    case object Thursday extends DayName("Thursday")
    case object Friday extends DayName("Friday")
    case object Saturday extends DayName("Saturday")

    val values: Seq[DayName] = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
  }

  object Day extends DayCompanion[Gregorian] with GregorianCalendarMember {
    override def names: Seq[DayName] = DayName.values

    override def apply(number: Int): Gregorian#Day = calendar.createDay(number)

    val epoch: Int = 1373429

    override val firstDayNumberInWeek: Int =
      (((Jewish.Day.firstDayNumberInWeek - 1) + (epoch % daysPerWeek)) % daysPerWeek) + 1
  }


  final class Moment(negative: Boolean, digits: List[Int])
    extends MomentBase(negative, digits) with GregorianCalendarMember
  {
    def morningHours(value: Int): Moment = firstHalfHours(value)

    def afternoonHours(value: Int): Moment = secondHalfHours(value)
  }


  object Moment extends MomentCompanion {
    override def apply(negative: Boolean, digits: List[Int]): Moment = new Moment(negative, digits)
  }
}


object Gregorian extends Gregorian
