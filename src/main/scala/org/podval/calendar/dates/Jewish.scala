package org.podval.calendar.dates

// TODO add a check that length of the year and total length of the months are the same
class Jewish private() extends Calendar[Jewish] {

  trait JewishCalendarMember extends CalendarMember[Jewish] {
    final override def calendar: Jewish = Jewish.this
  }

  final class Year(number: Int)
    extends YearBase(number) with JewishCalendarMember
  {
    require(0 < number)

    def newMoon: Moment = month(1).newMoon

    override def firstDayNumber: Int = {
      val correction =
        if (isAduCorrected) 1
        else if (isFirstCorrected) 1 + (if (isFirstAduCorrected) 1 /* KH 7:3 */ else 0 /* KH 7:2 */)
        else if (isSecondCorrected) 2  /* KH 7:4 */
        else if (isThirdCorrected ) 1  /* KH 7:5 */
        else 0

      newMoon.day.number + correction
    }

    def isAduCorrected: Boolean = Year.isAdu(newMoon.day)  // KH 7:1

    def isFirstCorrected: Boolean = !isAduCorrected && (newMoon.time >= Year.firstCorrection)

    def isFirstAduCorrected: Boolean = isFirstCorrected && Year.isAdu(newMoon.day.next)

    def isSecondCorrected: Boolean = !isAduCorrected && !isFirstCorrected &&
      ((newMoon.day.name == DayName.Shlishi) && newMoon.time >= Year.secondCorrection && !this.isLeap)

    // This is not defined for yer 0 - and doesn't apply :)
    def isThirdCorrected: Boolean = !isAduCorrected && !isFirstCorrected && !isSecondCorrected &&
      ((newMoon.day.name == DayName.Sheni) && newMoon.time >= Year.thirdCorrection && this.prev.isLeap)

    override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

    def cycle: Int = Year.cycle(number)

    def numberInCycle: Int = Year.numberInCycle(number)

    override def character: YearCharacter = (isLeap, kind)

    // KH 8:7,8
    def kind: YearKind = {
      val daysOverShort = lengthInDays - (if (isLeap) 383 else 353)

      daysOverShort match {
        case 0 => YearKind.Short
        case 1 => YearKind.Regular
        case 2 => YearKind.Full
        case _ => throw new IllegalArgumentException("Impossible year length " + lengthInDays + " for " + this)
      }
    }
  }

  sealed trait YearKind

  object YearKind {
    case object Short extends YearKind
    case object Regular extends YearKind
    case object Full extends YearKind

    val values: Seq[YearKind] = Seq(Short, Regular, Full)
  }

  override type YearCharacter = (Boolean, YearKind)

  object Year extends YearCompanion {

    override def apply(number: Int): Year = new Year(number)

    protected override def characters: Seq[YearCharacter] =
      for (isLeap <- Seq(true, false); kind <- YearKind.values) yield (isLeap, kind)

    // KH 8:5-6
    protected override def monthNamesAndLengths(character: YearCharacter): List[MonthNameAndLength] = {
      import MonthName._

      character match { case (isLeap: Boolean, kind: YearKind) =>
        List(
          MonthNameAndLength(Tishrei   , 30),
          MonthNameAndLength(Marheshvan, if (kind == YearKind.Full) 30 else 29),
          MonthNameAndLength(Kislev    , if (kind == YearKind.Short) 29 else 30),
          MonthNameAndLength(Teves     , 29),
          MonthNameAndLength(Shvat     , 30)
        ) ++
        (if (!isLeap)
          List(MonthNameAndLength(Adar, 29))
        else
          List(MonthNameAndLength(AdarI, 30), MonthNameAndLength(AdarII, 29))) ++
        List(
          MonthNameAndLength(Nisan , 30),
          MonthNameAndLength(Iyar  , 29),
          MonthNameAndLength(Sivan , 30),
          MonthNameAndLength(Tammuz, 29),
          MonthNameAndLength(Av    , 30),
          MonthNameAndLength(Elul  , 29)
        )
      }
    }

    private val adu: Set[Jewish#DayName] = Set(DayName.Rishon, DayName.Rvii, DayName.Shishi)

    def isAdu(day: Jewish#Day): Boolean = adu.contains(day.name)

    protected override def areYearsPositive: Boolean = true

    private[this] val leapYears: Set[Int] =
      Set(3, 6, 8, 11, 14, 17, 19) // TODO calculate Meton's cycle in the paper

    override def isLeap(yearNumber: Int): Boolean = leapYears.contains(numberInCycle(yearNumber))

    override def firstMonth(yearNumber: Int): Int = monthsInCycle*(cycle(yearNumber) - 1) + firstMonthInCycle(yearNumber)

    override def lengthInMonths(yearNumber: Int): Int = lengthInMonths(isLeap(yearNumber))

    def normal: TimeInterval = Month.meanLunarPeriod*lengthInMonths(isLeap = false)

    def leap: TimeInterval = Month.meanLunarPeriod*lengthInMonths(isLeap = true)

    def lengthInMonths(isLeap: Boolean): Int = if (isLeap) 13 else 12

    val yearsInCycle: Int = 19

    val leapYearsInCycle: Int = leapYears.size

    val monthsBeforeYearInCycle: Seq[Int] =
      ((1 to yearsInCycle) map lengthInMonths).scanLeft(0)(_ + _)

    val monthsInCycle: Int = monthsBeforeYearInCycle.last

    val cycleLength: TimeInterval = Month.meanLunarPeriod * monthsInCycle

    def firstMonthInCycle(yearNumber: Int): Int =
      monthsBeforeYearInCycle(numberInCycle(yearNumber) - 1) + 1

    def numberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1

    def cycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1

    // TODO meaningful names
    val firstCorrection  = interval.hours(18) // KH 7:1
    val secondCorrection = interval.hours(9).parts(204) // KH 7:4
    val thirdCorrection  = interval.hours(15).parts(589) // KH 7:5
  }


  final class Month(number: Int)
    extends MonthBase[Jewish](number) with JewishCalendarMember
  {
    def newMoon: Moment = Month.firstNewMoon + Month.meanLunarPeriod*(number-1)
  }


  sealed class MonthName(val name: String) extends Named(name)

  object MonthName {
    case object Tishrei extends MonthName("Tishrei")
    case object Marheshvan extends MonthName("Marcheshvan")
    case object Kislev extends MonthName("Kislev")
    case object Teves extends MonthName("Teves")
    case object Shvat extends MonthName("Shevat")
    case object Adar extends MonthName("Adar")
    case object Nisan extends MonthName("Nissan")
    case object Iyar extends MonthName("Iyar")
    case object Sivan extends MonthName("Sivan")
    case object Tammuz extends MonthName("Tammuz")
    case object Av extends MonthName("Av")
    case object Elul extends MonthName("Elul")
    case object AdarI extends MonthName("Adar I")
    case object AdarII extends MonthName("Adar II")
  }

  object Month extends MonthCompanion {
    override def apply(number: Int): Month = new Month(number)

    // KH 6:3
    val meanLunarPeriod = interval.days(29).hours(12).parts(793)  // TODO how is this really called? tropical?

    // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2): BeHaRaD: (KH 6:8)
    val firstNewMoon = moment.day(2).nightHours(5).parts(204)

    override def yearNumber(monthNumber: Int): Int = {
      val cycleOfMonth = ((monthNumber - 1) / Year.monthsInCycle) + 1
      val yearsBeforeCycle = (cycleOfMonth - 1) * Year.yearsInCycle
      val yearMonthIsInCycle = Year.monthsBeforeYearInCycle.count(_ < numberInCycleOfMonth(monthNumber))
      yearsBeforeCycle + yearMonthIsInCycle
    }

    override def numberInYear(monthNumber: Int): Int = numberInCycleOfMonth(monthNumber) - Year.firstMonthInCycle(yearNumber(monthNumber)) + 1

    private def numberInCycleOfMonth(monthNumber: Int): Int = ((monthNumber - 1) % Year.monthsInCycle) + 1
  }


  final class Day(number: Int)
    extends DayBase[Jewish](number) with JewishCalendarMember

  sealed class DayName(name: String) extends Named(name)

  object DayName {
    case object Rishon extends DayName("Rishon")
    case object Sheni extends DayName("Sheni")
    case object Shlishi extends DayName("Shlishi")
    case object Rvii extends DayName("Rvii")
    case object Chamishi extends DayName("Chamishi")
    case object Shishi extends DayName("Shishi")
    case object Shabbos extends DayName("Shabbos")

    val values: Seq[DayName] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)
  }

  object Day extends DayCompanion[Jewish] with JewishCalendarMember {
    override def names: Seq[DayName] = DayName.values

    override def apply(number: Int): Day = new Day(number)

    // It seems that first day of the first year was Sunday; molad - BaHaRad.
    // Second year - friday; molad - 8 in the morning.
    override val firstDayNumberInWeek: Int = 1
  }


  final class Moment(negative: Boolean, digits: List[Int])
    extends MomentBase(negative, digits) with JewishCalendarMember
  {
    def nightHours(value: Int): Moment = firstHalfHours(value)

    def dayHours(value: Int): Moment = secondHalfHours(value)
  }


  object Moment extends MomentCompanion {
    override def apply(negative: Boolean, digits: List[Int]): Moment = new Moment(negative, digits)
  }
}


object Jewish extends Jewish
