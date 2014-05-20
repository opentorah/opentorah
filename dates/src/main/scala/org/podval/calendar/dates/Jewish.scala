/*
 * Copyright 2011-2014 Podval Group.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.calendar.dates


object Jewish extends Calendar {

  final class Year(number: Int) extends YearBase(number) {

    require(0 < number)


    // TODO externalize the checks
    override def firstDay: Int = {
      val newMoon = month(1).newMoon
      val day = newMoon.day
      val time = newMoon.time

      if (Year.isAdu(day.name)) day.next // KH 7:1
      else if (time >= Year.firstCorrection) {
        if (!Year.isAdu(day.next.name)) day.next /* KH 7:2 */ else day.next.next /* KH 7:3 */
      }
      else if ((day.name == Day.Shlishi) && time >= Year.secondCorrection && !this     .isLeap) day.next.next /* KH 7:4 */
      else if ((day.name == Day.Sheni  ) && time >= Year.thirdCorrection  &&  this.prev.isLeap) day.next /* KH 7:5 */
      else day
    }.number


    override def lengthInDays: Int = next.firstDay - this.firstDay


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


    def newMoon: Moment = month(1).newMoon
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
          List(MonthNameAndLength(AdarI, 30), MonthNameAndLength(AdarII, 30))) ++
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


    def isAdu(dayName: Day.Name) = adu.contains(dayName)


    protected override def areYearsPositive: Boolean = true


    private[this] val leapYears = Set(3, 6, 8, 11, 14, 17, 19) // TODO calculate Menton's cycle in the paper


    override def isLeap(yearNumber: Int) = leapYears.contains(numberInCycle(yearNumber))


    override def firstMonth(yearNumber: Int): Int = monthsInCycle*(cycle(yearNumber) - 1) + firstMonthInCycle(yearNumber)


    override def lengthInMonths(yearNumber: Int): Int = if (isLeap(yearNumber)) 13 else 12


    val yearsInCycle = 19


    val monthsBeforeYearInCycle = ((1 to yearsInCycle) map (lengthInMonths(_))).scanLeft(0)(_ + _)


    val monthsInCycle = monthsBeforeYearInCycle.last


    def firstMonthInCycle(yearNumber: Int): Int = monthsBeforeYearInCycle(numberInCycle(yearNumber) - 1) + 1


    def numberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1


    def cycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1


    // TODO meaningful names
    val firstCorrection  = hours(18) // KH 7:1
    val secondCorrection = hours(9).parts(204) // KH 7:4
    val thirdCorrection  = hours(15).parts(589) // KH 7:5
  }



  val Year = new YearCompanion


  final class Month(number: Int) extends MonthBase(number) {

    def newMoon: Moment = Month.firstNewMoon + Month.meanLunarPeriod*(number-1)
  }


  object Month extends MonthCompanion {

    override def apply(number: Int): Month = new Month(number)


    sealed class Name(name: String) extends Named(name)

    case object Tishrei    extends Name("Tishrei")
    case object Marheshvan extends Name("Marheshvan")
    case object Kislev     extends Name("Kislev")
    case object Teves      extends Name("Teves")
    case object Shvat      extends Name("Shvat")
    case object Adar       extends Name("Adar")
    case object Nisan      extends Name("Nisan")
    case object Iyar       extends Name("Iyar")
    case object Sivan      extends Name("Sivan")
    case object Tammuz     extends Name("Tammuz")
    case object Av         extends Name("Av")
    case object Elul       extends Name("Elul")
    case object AdarI      extends Name("Adar I")
    case object AdarII     extends Name("Adar II")


    // KH 6:3
    val meanLunarPeriod = days(29).hours(12).parts(793)  // TODO how is this really called? tropical?


    // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2): BeHaRaD: (KH 6:8)
    val firstNewMoon = day(2).nightHours(5).parts(204)


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


  final class Moment(inParts: Long) extends MomentBase(inParts) {

    def nightHours(value: Int): Moment = firstHalfHours(value)


    def dayHours(value: Int): Moment = secondHalfHours(value)
  }


  object Moment extends MomentCompanion {

    override def apply(inParts: Long): Moment = new Moment(inParts)
  }
}
