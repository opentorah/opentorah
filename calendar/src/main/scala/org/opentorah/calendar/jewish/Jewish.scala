package org.opentorah.calendar.jewish

import org.opentorah.calendar.{Calendar, Week}
import org.opentorah.metadata.{LanguageSpec, Named, Names}

object Jewish extends Calendar {

  override def epoch: Int = 0

  override def epochHours: Int = 0

  // first day of the Jewish calendar epoch -
  // first day of the year 1 and the day of molad BaHaRaD -
  // was a Monday:
  def epochDayNumberInWeek: Int = 2

  final class JewishYear(number: Int) extends YearBase(number) {
    require(0 <= number)

    def newMoon: Moment = month(1).newMoon

    def newYearDelay: NewYear.Delay = newYearDelay(newMoon)

    private def newYearDelay(newMoon: Moment): NewYear.Delay = NewYear.delay(number, newMoon)

    override def firstDayNumber: Int = {
      val newMoon: Moment = this.newMoon
      newMoon.dayNumber + newYearDelay(newMoon).days
    }

    override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

    override def character: YearCharacter = (isLeap, kind)

    def kind: Year.Kind = Year.kind(isLeap, lengthInDays)

    def latestAdar: Month = month(if (isLeap) Month.AdarII else Month.Adar)

    def isShemittah: Boolean = (number % 7) == 0
  }

  override type Year = JewishYear

  override type YearCharacter = (Boolean, Year.Kind)

  override protected def areYearsPositive: Boolean = true

  final class JewishYearCompanion extends YearCompanion {
    type Kind = JewishYearCompanion.Kind

    val Kind: JewishYearCompanion.Kind.type = JewishYearCompanion.Kind

    override protected def newYear(number: Int): Year = new JewishYear(number)

    override protected def characters: Seq[YearCharacter] =
      for (isLeap <- Seq(true, false); kind <- Kind.values) yield (isLeap, kind)

    // KH 8:5-6
    override protected def monthNamesAndLengths(character: YearCharacter): Seq[MonthNameAndLength] = {
      character match { case (isLeap: Boolean, kind: Kind) =>
        Seq(
          new MonthNameAndLength(Month.Tishrei   , 30),
          new MonthNameAndLength(Month.Marheshvan, if (kind == Kind.Full) 30 else 29),
          new MonthNameAndLength(Month.Kislev    , if (kind == Kind.Short) 29 else 30),
          new MonthNameAndLength(Month.Teves     , 29),
          new MonthNameAndLength(Month.Shvat     , 30)
        ) ++
          (if (!isLeap)
            Seq(new MonthNameAndLength(Month.Adar, 29))
          else
            Seq(new MonthNameAndLength(Month.AdarI, 30), new MonthNameAndLength(Month.AdarII, 29))) ++
          Seq(
            new MonthNameAndLength(Month.Nisan , 30),
            new MonthNameAndLength(Month.Iyar  , 29),
            new MonthNameAndLength(Month.Sivan , 30),
            new MonthNameAndLength(Month.Tammuz, 29),
            new MonthNameAndLength(Month.Av    , 30),
            new MonthNameAndLength(Month.Elul  , 29)
          )
      }
    }

    override def isLeap(yearNumber: Int): Boolean = LeapYearsCycle.isLeapYear(yearNumber)

    override def firstMonth(yearNumber: Int): Int = LeapYearsCycle.firstMonth(yearNumber)

    override def lengthInMonths(yearNumber: Int): Int = LeapYearsCycle.yearLengthInMonths(yearNumber)

    // KH 8:7-8
    val shortNonLeapYearLength: Int = yearLength((false, Kind.Short)) //353
    val shortLeapYearLength: Int = yearLength((true, Kind.Short)) // 383

    def kind(isLeap: Boolean, lengthInDays: Int): Kind = {
      val daysOverShort: Int = lengthInDays - (if (isLeap) shortLeapYearLength else shortNonLeapYearLength)

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
  }

  override lazy val Year = new JewishYearCompanion

  trait JewishMonth extends MonthBase {
    def newMoon: Moment = Moon.newMoon(number)
  }

  final override type Month = JewishMonth

  sealed trait JewishMonthName extends Named {
    final override def names: Names = Month.toNames(this)
  }

  override type MonthName = JewishMonthName

  final class JewishMonthCompanion extends MonthCompanion {
    private[opentorah] override def apply(yearOption: Option[Year], monthNumber: Int): Month =
      new JewishMonth {
        override protected var yearOpt: Option[Year] = yearOption
        override def number: Int = monthNumber
      }

    override private[opentorah] def yearNumber(monthNumber: Int): Int = LeapYearsCycle.monthYear(monthNumber)

    override private[opentorah] def numberInYear(monthNumber: Int): Int = LeapYearsCycle.monthNumberInYear(monthNumber)

    case object Tishrei    extends JewishMonthName
    case object Marheshvan extends JewishMonthName
    case object Kislev     extends JewishMonthName
    case object Teves      extends JewishMonthName
    case object Shvat      extends JewishMonthName
    case object Adar       extends JewishMonthName
    case object Nisan      extends JewishMonthName
    case object Iyar       extends JewishMonthName
    case object Sivan      extends JewishMonthName
    case object Tammuz     extends JewishMonthName
    case object Av         extends JewishMonthName
    case object Elul       extends JewishMonthName
    case object AdarI      extends JewishMonthName { override def name: String = "Adar I"}
    case object AdarII     extends JewishMonthName { override def name: String = "Adar II"}

    override val values: Seq[Key] =
      Seq(Tishrei, Marheshvan, Kislev, Teves, Shvat, Adar, Nisan, Iyar, Sivan, Tammuz, Av, Elul, AdarI, AdarII)

    protected override def resourceName: String = "JewishMonth"
  }

  override lazy val Month: JewishMonthCompanion = new JewishMonthCompanion

  trait JewishDay extends DayBase {
    def isShabbos: Boolean = is(Week.Day.Shabbos)

    def roshChodeshOf: Option[Month.Name] =
      if (numberInMonth == 1) Some(month.name) else
        if (numberInMonth == 30) Some(month.next.name)
        else None

    def isRoshChodesh: Boolean = roshChodeshOf.isDefined

    def isShabbosMevarchim: Boolean = isShabbos && (shabbosAfter.month != this.month)

    def shabbosAfter: Day = next.next(Week.Day.Shabbos)

    def shabbosBefore: Day = prev.prev(Week.Day.Shabbos)
  }

  override type Day = JewishDay

  final class JewishDayCompanion extends DayCompanion {
    private[opentorah] override def apply(monthOption: Option[Month], dayNumber: Int): Day =
      new JewishDay {
        override protected var monthOpt: Option[Month] = monthOption
        override def number: Int = dayNumber
      }
  }

  override lazy val Day: JewishDayCompanion = new JewishDayCompanion

  final class JewishMoment(digits: Seq[Int]) extends MomentBase(digits) {
    override def companion: JewishMomentCompanion = Point

    def nightHours(value: Int): Moment = firstHalfHours(value)

    def dayHours(value: Int): Moment = secondHalfHours(value)
  }

  override type Moment = JewishMoment

  final class JewishMomentCompanion extends MomentCompanion {
    override protected def newNumber(digits: Seq[Int]): Point = new JewishMoment(digits)
  }

  override lazy val Point: JewishMomentCompanion = new JewishMomentCompanion

  override def Moment: JewishMomentCompanion = Point

  override def inToString(number: Int)(implicit spec: LanguageSpec): String = spec.toString(number)
}
