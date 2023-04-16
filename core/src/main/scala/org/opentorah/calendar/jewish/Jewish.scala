package org.opentorah.calendar.jewish

import org.opentorah.calendar.{Calendar, Week}
import org.opentorah.metadata.{Language, Named, Names}

object Jewish extends Calendar:

  override def name: String = "jewish"

  override def epoch: Int = 0

  override def epochHours: Int = 0

  // first day of the Jewish calendar epoch -
  // first day of the year 1 and the day of molad BaHaRaD -
  // was a Monday:
  def epochDayNumberInWeek: Int = 2

  final class JewishYear(number: Int) extends YearBase(number):
    def newMoon: Moment = month(1).newMoon

    def newYearDelay: NewYear.Delay = newYearDelay(newMoon)

    private def newYearDelay(newMoon: Moment): NewYear.Delay = NewYear.delay(number, newMoon)

    override def firstDayNumber: Int =
      val newMoon: Moment = this.newMoon
      newMoon.dayNumber + newYearDelay(newMoon).days

    override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

    override def character: YearCharacter = (isLeap, kind)

    def kind: Year.Kind = Year.kind(isLeap, lengthInDays)

    def latestAdar: Month = month(if isLeap then Month.AdarII else Month.Adar)

    def isShemittah: Boolean = (number % 7) == 0

  override type Year = JewishYear

  override type YearCharacter = (Boolean, Year.Kind)

  final class JewishYearCompanion extends YearCompanion:
    type Kind = JewishYearCompanion.Kind

    val Kind: JewishYearCompanion.Kind.type = JewishYearCompanion.Kind

    override protected def newYear(number: Int): Year = JewishYear(number)

    override protected def characters: Seq[YearCharacter] =
      for isLeap: Boolean <- Seq(true, false); kind <- Kind.values yield (isLeap, kind)

    // KH 8:5-6
    override protected def monthNamesAndLengths(character: YearCharacter): Seq[MonthNameAndLength] = character match
      case (isLeap: Boolean, kind: Kind) =>
        Seq(
          MonthNameAndLength(Month.Tishrei   , 30),
          MonthNameAndLength(Month.Marheshvan, if kind == Kind.Full  then 30 else 29),
          MonthNameAndLength(Month.Kislev    , if kind == Kind.Short then 29 else 30),
          MonthNameAndLength(Month.Teves     , 29),
          MonthNameAndLength(Month.Shvat     , 30)
        ) ++ (
          if !isLeap
          then Seq(MonthNameAndLength(Month.Adar , 29))
          else Seq(MonthNameAndLength(Month.AdarI, 30), MonthNameAndLength(Month.AdarII, 29))
        ) ++ Seq(
          MonthNameAndLength(Month.Nisan , 30),
          MonthNameAndLength(Month.Iyar  , 29),
          MonthNameAndLength(Month.Sivan , 30),
          MonthNameAndLength(Month.Tammuz, 29),
          MonthNameAndLength(Month.Av    , 30),
          MonthNameAndLength(Month.Elul  , 29)
        )

    override def isLeap(yearNumber: Int): Boolean = LeapYearsCycle.isLeapYear(yearNumber)

    override def firstMonth(yearNumber: Int): Int = LeapYearsCycle.firstMonth(yearNumber)

    override def lengthInMonths(yearNumber: Int): Int = LeapYearsCycle.yearLengthInMonths(yearNumber)

    // KH 8:7-8
    // lazy to make initialization work
    lazy val shortNonLeapYearLength: Int = yearLength((false, Kind.Short)) //353
    lazy val shortLeapYearLength: Int = yearLength((true, Kind.Short)) // 383

    def kind(isLeap: Boolean, lengthInDays: Int): Kind =
      val daysOverShort: Int = lengthInDays - (if isLeap then shortLeapYearLength else shortNonLeapYearLength)

      daysOverShort match
        case 0 => Kind.Short
        case 1 => Kind.Regular
        case 2 => Kind.Full
        case _ => throw IllegalArgumentException("Impossible year length " + lengthInDays + " for " + this)

  object JewishYearCompanion:
    // KH 8:6
    enum Kind derives CanEqual:
      case Short
      case Regular
      case Full

  override type YearCompanionType = JewishYearCompanion

  override protected def createYearCompanion: YearCompanionType = new JewishYearCompanion

  final class JewishMonth(yearOptInitial: Option[Year], monthNumber: Int) extends MonthBase(yearOptInitial, monthNumber):
    def newMoon: Moment = Moon.newMoon(number)

  final override type Month = JewishMonth

  sealed class JewishMonthName(nameOverride: Option[String] = None) extends MonthNameBase(nameOverride)

  override type MonthName = JewishMonthName

  object JewishMonthName extends MonthCompanion(resourceName = "JewishMonth"):
    private[opentorah] override def apply(yearOption: Option[Year], monthNumber: Int): Month =
      JewishMonth(yearOption, monthNumber)

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
    case object AdarI      extends JewishMonthName(nameOverride = Some("Adar I"))
    case object AdarII     extends JewishMonthName(nameOverride = Some("Adar II"))

    override val valuesSeq: Seq[Name] =
      Seq(Tishrei, Marheshvan, Kislev, Teves, Shvat, Adar, Nisan, Iyar, Sivan, Tammuz, Av, Elul, AdarI, AdarII)

  override type MonthCompanionType = JewishMonthName.type

  override protected def createMonthCompanion: MonthCompanionType = JewishMonthName

  final class JewishDay(monthOption: Option[Month], dayNumber: Int) extends DayBase(monthOption, dayNumber):
    def isShabbos: Boolean = is(Week.Day.Shabbos)

    def roshChodeshOf: Option[Month.Name] =
      if numberInMonth == 1 then Some(month.name) else
      if numberInMonth == 30 then Some(month.next.name)
      else None

    def isRoshChodesh: Boolean = roshChodeshOf.isDefined

    def isShabbosMevarchim: Boolean = isShabbos && (shabbosAfter.month != this.month)

    def shabbosAfter: Day = next.nextDay(Week.Day.Shabbos)

    def shabbosBefore: Day = prev.prevDay(Week.Day.Shabbos)

  override type Day = JewishDay

  override protected def newDay(monthOption: Option[Month], dayNumber: Int): Day = JewishDay(monthOption, dayNumber)

  final class JewishMoment(digits: Seq[Int]) extends MomentBase(digits):
    def nightHours(value: Int): Moment = firstHalfHours(value)

    def dayHours(value: Int): Moment = secondHalfHours(value)

  override type Moment = JewishMoment

  override protected def newPoint(digits: Seq[Int]): Point = JewishMoment(digits)

  override def intToString(number: Int)(using spec: Language.Spec): String = spec.toString(number)
