package org.podval.calendar.dates

class Gregorian private() extends Calendar[Gregorian] {

  final class Year(number: Int) extends YearBase(number) {
    override def firstDayNumber: Int = Year.firstDay(number)

    override def lengthInDays: Int = Year.lengthInDays(number)

    override def character: Year.Character = isLeap
  }


  final class YearCompanion extends YearCompanionBase {
    type Character = Boolean

    override def apply(number: Int): Year = new Year(number)

    protected override def characters: Seq[Year.Character] = Seq(true, false)

    protected override def monthNamesAndLengths(isLeap: Year.Character): List[MonthNameAndLength] = {
      import Month._
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

    val monthsInYear = 12

    private val daysInNonLeapYear = 365

    def firstDay(yearNumber: Int): Int = daysInNonLeapYear * (yearNumber - 1) + (yearNumber - 1)/4 - (yearNumber - 1)/100 + (yearNumber - 1)/400 + 1

    def lengthInDays(yearNumber: Int): Int = if (Gregorian.Year.isLeap(yearNumber)) daysInNonLeapYear + 1 else daysInNonLeapYear
  }


  final class Month(number: Int) extends MonthBase(number)


  object Month extends MonthCompanion {
    override def apply(number: Int): Month = new Month(number)

    sealed class Name(name: String) extends Named(name)

    case object January   extends Name("January")
    case object February  extends Name("February")
    case object March     extends Name("March")
    case object April     extends Name("April")
    case object May       extends Name("May")
    case object June      extends Name("June")
    case object July      extends Name("July")
    case object August    extends Name("August")
    case object September extends Name("September")
    case object October   extends Name("October")
    case object November  extends Name("November")
    case object December  extends Name("December")

    override def yearNumber(monthNumber: Int): Int = (monthNumber - 1) / Gregorian.Year.monthsInYear + 1

    override def numberInYear(monthNumber: Int): Int =  monthNumber - Gregorian.Year.firstMonth(yearNumber(monthNumber)) + 1
  }


  final class Day(number: Int) extends DayBase(number)


  object Day extends DayCompanion {

    sealed class Name(name: String) extends Named(name)

    case object Sunday    extends Name("Sunday")
    case object Monday    extends Name("Monday")
    case object Tuesday   extends Name("Tuesday")
    case object Wednesday extends Name("Wednesday")
    case object Thursday  extends Name("Thursday")
    case object Friday    extends Name("Friday")
    case object Saturday  extends Name("Saturday")

    def names: Seq[Name] = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)

    override def apply(number: Int): Day = new Day(number)

    val epoch = 1373429

    override val firstDayNumberInWeek: Int =
      (((Jewish.Day.firstDayNumberInWeek - 1) + (epoch % daysPerWeek)) % daysPerWeek) + 1
  }


  final class Moment(negative: Boolean, digits: List[Int]) extends MomentBase(negative, digits) {
    def morningHours(value: Int): Moment = firstHalfHours(value)

    def afternoonHours(value: Int): Moment = secondHalfHours(value)
  }


  object Moment extends MomentCompanion {
    override def apply(negative: Boolean, digits: List[Int]): Moment = new Moment(negative, digits)
  }

  override val Year = new YearCompanion
}


object Gregorian extends Gregorian
