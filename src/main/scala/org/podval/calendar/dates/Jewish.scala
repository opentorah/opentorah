package org.podval.calendar.dates

// TODO add a check that length of the year and total length of the months are the same
class Jewish private() extends Calendar[Jewish] {

  final class Year(number: Int) extends YearBase(number) {
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
      ((newMoon.day.name == Day.Shlishi) && newMoon.time >= Year.secondCorrection && !this.isLeap)

    // This is not defined for yer 0 - and doesn't apply :)
    def isThirdCorrected: Boolean = !isAduCorrected && !isFirstCorrected && !isSecondCorrected &&
      ((newMoon.day.name == Day.Sheni) && newMoon.time >= Year.thirdCorrection && this.prev.isLeap)

    override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

    def cycle: Int = Year.cycle(number)

    def numberInCycle: Int = Year.numberInCycle(number)

    override def character: Year.Character = (isLeap, kind)

    // KH 8:7,8
    def kind: Year.Kind = {
      val daysOverShort = lengthInDays - (if (isLeap) 383 else 353)

      daysOverShort match {
        case 0 => Year.Short
        case 1 => Year.Regular
        case 2 => Year.Full
        case _ => throw new IllegalArgumentException("Impossible year length " + lengthInDays + " for " + this)
      }
    }
  }


  final class YearCompanion extends YearCompanionBase {

    sealed trait Kind
    case object Short   extends Year.Kind
    case object Regular extends Year.Kind
    case object Full    extends Year.Kind


    type Character = (Boolean, Year.Kind)


    override def apply(number: Int): Year = new Year(number)

    protected override def characters: Seq[Year.Character] =
      for (isLeap <- Seq(true, false); kind <- Seq(Short, Regular, Full)) yield (isLeap, kind)

    // KH 8:5-6
    protected override def monthNamesAndLengths(character: Year.Character): List[MonthNameAndLength] = {
      import Month._

      character match { case (isLeap: Boolean, kind: Year.Kind) =>
        List(
          MonthNameAndLength(Tishrei   , 30),
          MonthNameAndLength(Marheshvan, if (kind == Full) 30 else 29),
          MonthNameAndLength(Kislev    , if (kind == Short) 29 else 30),
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

    private val adu: Set[Day.Name] = Set(Day.Rishon, Day.Rvii, Day.Shishi)

    def isAdu(day: Day): Boolean = adu.contains(day.name)

    protected override def areYearsPositive: Boolean = true

    private[this] val leapYears = Set(3, 6, 8, 11, 14, 17, 19) // TODO calculate Meton's cycle in the paper

    override def isLeap(yearNumber: Int): Boolean = leapYears.contains(numberInCycle(yearNumber))

    override def firstMonth(yearNumber: Int): Int = monthsInCycle*(cycle(yearNumber) - 1) + firstMonthInCycle(yearNumber)

    override def lengthInMonths(yearNumber: Int): Int = lengthInMonths(isLeap(yearNumber))

    def normal: TimeInterval = Month.meanLunarPeriod*lengthInMonths(isLeap = false)

    def leap: TimeInterval = Month.meanLunarPeriod*lengthInMonths(isLeap = true)

    def lengthInMonths(isLeap: Boolean): Int = if (isLeap) 13 else 12

    val yearsInCycle = 19

    val leapYearsInCycle: Int = leapYears.size

    val monthsBeforeYearInCycle = ((1 to yearsInCycle) map lengthInMonths).scanLeft(0)(_ + _)

    val monthsInCycle: Int = monthsBeforeYearInCycle.last

    val cycleLength: TimeInterval = Month.meanLunarPeriod * monthsInCycle

    def firstMonthInCycle(yearNumber: Int): Int = monthsBeforeYearInCycle(numberInCycle(yearNumber) - 1) + 1

    def numberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1

    def cycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1

    // TODO meaningful names
    val firstCorrection  = interval.hours(18) // KH 7:1
    val secondCorrection = interval.hours(9).parts(204) // KH 7:4
    val thirdCorrection  = interval.hours(15).parts(589) // KH 7:5
  }


  final class Month(number: Int) extends MonthBase(number) {
    def newMoon: Moment = Month.firstNewMoon + Month.meanLunarPeriod*(number-1)
  }


  object Month extends MonthCompanion {
    override def apply(number: Int): Month = new Month(number)


    sealed class Name(val name: String) extends Named(name)

    case object Tishrei    extends Name("Tishrei")
    case object Marheshvan extends Name("Marcheshvan")
    case object Kislev     extends Name("Kislev")
    case object Teves      extends Name("Teves")
    case object Shvat      extends Name("Shevat")
    case object Adar       extends Name("Adar")
    case object Nisan      extends Name("Nissan")
    case object Iyar       extends Name("Iyar")
    case object Sivan      extends Name("Sivan")
    case object Tammuz     extends Name("Tammuz")
    case object Av         extends Name("Av")
    case object Elul       extends Name("Elul")
    case object AdarI      extends Name("Adar I")
    case object AdarII     extends Name("Adar II")


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


  final class Day(number: Int) extends DayBase(number)


  object Day extends DayCompanion {

    sealed class Name(name: String) extends Named(name)

    case object Rishon   extends Name("Rishon")
    case object Sheni    extends Name("Sheni")
    case object Shlishi  extends Name("Shlishi")
    case object Rvii     extends Name("Rvii")
    case object Chamishi extends Name("Chamishi")
    case object Shishi   extends Name("Shishi")
    case object Shabbos  extends Name("Shabbos")


    def names: Seq[Name] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)

    override def apply(number: Int): Day = new Day(number)

    // It seems that first day of the first year was Sunday; molad - BaHaRad.
    // Second year - friday; molad - 8 in the morning.
    override val firstDayNumberInWeek: Int = 1
  }


  final class Moment(negative: Boolean, digits: List[Int]) extends MomentBase(negative, digits) {
    def nightHours(value: Int): Moment = firstHalfHours(value)

    def dayHours(value: Int): Moment = secondHalfHours(value)
  }


  object Moment extends MomentCompanion {
    override def apply(negative: Boolean, digits: List[Int]): Moment = new Moment(negative, digits)
  }

  final override val Year = new YearCompanion
}


object Jewish extends Jewish
