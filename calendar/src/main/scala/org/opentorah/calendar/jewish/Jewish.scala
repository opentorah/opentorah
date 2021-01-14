package org.opentorah.calendar.jewish

import org.opentorah.metadata.{LanguageSpec, Named, NamedCompanion, Names}
import org.opentorah.dates.Calendar

object Jewish extends Calendar {

  final class JewishYear(number: Int) extends YearBase(number) {
    require(0 <= number)

    def newMoon: Moment = month(1).newMoon

    def newYearDelay: NewYear.Delay = newYearDelay(newMoon)

    private def newYearDelay(newMoon: Moment): NewYear.Delay = NewYear.delay(number, newMoon)

    override def firstDayNumber: Int = {
      val nm = newMoon
      nm.dayNumber + newYearDelay(nm).days
    }

    override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

    override def character: YearCharacter = (isLeap, kind)

    def kind: Year.Kind = Year.kind(isLeap, lengthInDays)

    def latestAdar: Month = month(if (isLeap) Month.Name.AdarII else Month.Name.Adar)

    def isShemittah: Boolean = (number % 7) == 0
  }

  final override type Year = JewishYear

  final override type YearCharacter = (Boolean, Year.Kind)

  trait JewishYearCompanion extends YearCompanion { this: YearCompanion =>
    final type Kind = JewishYearCompanion.Kind

    final val Kind: JewishYearCompanion.Kind.type = JewishYearCompanion.Kind

    protected final override def characters: Seq[YearCharacter] =
      for (isLeap <- Seq(true, false); kind <- Kind.values) yield (isLeap, kind)

    // KH 8:5-6
    protected final override def monthNamesAndLengths(character: YearCharacter): Seq[MonthNameAndLength] = {
      character match { case (isLeap: Boolean, kind: Kind) =>
        Seq(
          new MonthNameAndLength(Month.Name.Tishrei   , 30),
          new MonthNameAndLength(Month.Name.Marheshvan, if (kind == Kind.Full) 30 else 29),
          new MonthNameAndLength(Month.Name.Kislev    , if (kind == Kind.Short) 29 else 30),
          new MonthNameAndLength(Month.Name.Teves     , 29),
          new MonthNameAndLength(Month.Name.Shvat     , 30)
        ) ++
          (if (!isLeap)
            Seq(new MonthNameAndLength(Month.Name.Adar, 29))
          else
            Seq(new MonthNameAndLength(Month.Name.AdarI, 30), new MonthNameAndLength(Month.Name.AdarII, 29))) ++
          Seq(
            new MonthNameAndLength(Month.Name.Nisan , 30),
            new MonthNameAndLength(Month.Name.Iyar  , 29),
            new MonthNameAndLength(Month.Name.Sivan , 30),
            new MonthNameAndLength(Month.Name.Tammuz, 29),
            new MonthNameAndLength(Month.Name.Av    , 30),
            new MonthNameAndLength(Month.Name.Elul  , 29)
          )
      }
    }

    protected final override def areYearsPositive: Boolean = true

    final override def isLeap(yearNumber: Int): Boolean = LeapYearsCycle.isLeapYear(yearNumber)

    final override def firstMonth(yearNumber: Int): Int = LeapYearsCycle.firstMonth(yearNumber)

    final override def lengthInMonths(yearNumber: Int): Int = LeapYearsCycle.yearLengthInMonths(yearNumber)

    // KH 8:7-8
    val shortNonLeapYearLength: Int = yearLength((false, Kind.Short)) //353
    val shortLeapYearLength: Int = yearLength((true, Kind.Short)) // 383

    final def kind(isLeap: Boolean, lengthInDays: Int): Kind = {
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

  final override lazy val Year = new JewishYearCompanion {
    protected override def newYear(number: Int): Year = new JewishYear(number)
  }

  trait JewishMonth extends MonthBase {
    def newMoon: Moment = Moon.newMoon(number)
  }

  final override type Month = JewishMonth

  trait JewishMonthCompanion extends MonthCompanion { this: MonthCompanion =>
    final override val Name: JewishMonthCompanion.type = JewishMonthCompanion

    private[opentorah] final override def yearNumber(monthNumber: Int): Int = LeapYearsCycle.monthYear(monthNumber)

    private[opentorah] final override def numberInYear(monthNumber: Int): Int = LeapYearsCycle.monthNumberInYear(monthNumber)
  }

  object JewishMonthCompanion extends NamedCompanion {
    sealed trait Key extends Named {
      final override def names: Names = toNames(this)
    }

    case object Tishrei extends Key
    case object Marheshvan extends Key
    case object Kislev extends Key
    case object Teves extends Key
    case object Shvat extends Key
    case object Adar extends Key
    case object Nisan extends Key
    case object Iyar extends Key
    case object Sivan extends Key
    case object Tammuz extends Key
    case object Av extends Key
    case object Elul extends Key
    case object AdarI extends Key { override def name: String = "Adar I"}
    case object AdarII extends Key { override def name: String = "Adar II"}

    override val values: Seq[Key] =
      Seq(Tishrei, Marheshvan, Kislev, Teves, Shvat, Adar, Nisan, Iyar, Sivan, Tammuz, Av, Elul, AdarI, AdarII)

    protected override def resourceName: String = "JewishMonth"
  }

  final override lazy val Month = new JewishMonthCompanion {
    private[opentorah] override def apply(yearOption: Option[Year], monthNumber: Int): Month =
      new JewishMonth {
        override protected var yearOpt: Option[Year] = yearOption
        override def number: Int = monthNumber
      }
  }

  trait JewishDay extends DayBase {
    def isShabbos: Boolean = is(Day.Name.Shabbos)

    def roshChodeshOf: Option[Month.Name] =
      if (numberInMonth == 1) Some(month.name) else
        if (numberInMonth == 30) Some(month.next.name)
        else None

    def isRoshChodesh: Boolean = roshChodeshOf.isDefined

    def isShabbosMevarchim: Boolean = isShabbos && (shabbosAfter.month != this.month)

    def shabbosAfter: Day = next.next(Day.Name.Shabbos)

    def shabbosBefore: Day = prev.prev(Day.Name.Shabbos)
  }

  final override type Day = JewishDay

  trait JewishDayCompanion extends DayCompanion {
    final override val Name: JewishDayCompanion.type = JewishDayCompanion

    final override def names: Seq[Name] = JewishDayCompanion.values

    final override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekJewish
  }

  object JewishDayCompanion extends NamedCompanion {
    sealed trait Key extends Named {
      final override def names: Names = toNames(this)
    }

    case object Rishon extends Key
    case object Sheni extends Key
    case object Shlishi extends Key
    case object Rvii extends Key
    case object Chamishi extends Key
    case object Shishi extends Key
    case object Shabbos extends Key

    override val values: Seq[Key] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)

    protected override def resourceName: String = "JewishDay"
  }

  final override lazy val Day = new JewishDayCompanion {
    private[opentorah] override def apply(monthOption: Option[Month], dayNumber: Int): Day =
      new JewishDay {
        override protected var monthOpt: Option[Month] = monthOption
        override def number: Int = dayNumber
      }
  }

  abstract class JewishMoment(digits: Seq[Int]) extends MomentBase(digits) {
    final def nightHours(value: Int): Moment = firstHalfHours(value)

    final def dayHours(value: Int): Moment = secondHalfHours(value)
  }

  final override type Point = JewishMoment

  trait JewishMomentCompanion extends MomentCompanion { this: MomentCompanion =>
  }

  final override lazy val Point: JewishMomentCompanion = new JewishMomentCompanion {
    protected override def newNumber(digits: Seq[Int]): Point =
      new JewishMoment(digits) {
        final override def companion: JewishMomentCompanion = Point
      }
  }

  final override def toString(number: Int)(implicit spec: LanguageSpec): String = spec.toString(number)
}
