/*
 * Copyright 2011-2013 Podval Group.
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

  // XXX assignments of the companion objects have to happen early on, but even this is not sufficient!
  // I found that I need to assign JewishCalendar to a val to trigger its initialization - or I end up with a null for the Year companion object!
  override protected val timeCompanion = Time
  override protected val momentCompanion = Moment
  override protected val dayCompanion = Day
  override protected val monthCompanion = Month
  override protected val yearCompanion = Year


  protected override val helper: JewishHelper.type = JewishHelper


  final class Year(number: Int) extends YearBase(number) {

    require(0 < number)


    // TODO give names to constants
    override def firstDay: Int = {
      val newMoon = month(1).newMoon
      val day = newMoon.day
      val time = newMoon.time

      if (Year.isAdu(day.name)) day.next // KH 7:1
      else if (time >= Time(18, 0)) {
        if (!Year.isAdu(day.next.name)) day.next /* KH 7:2 */ else day.next.next /* KH 7:3 */
      }
      else if ((day.name == Day.Shlishi) && time >= Time( 9, 204) && !this     .isLeap) day.next.next /* KH 7:4 */
      else if ((day.name == Day.Sheni  ) && time >= Time(15, 589) &&  this.prev.isLeap) day.next /* KH 7:5 */
      else day
    }.number


    override def lengthInDays: Int = next.firstDay - this.firstDay


    def cycle: Int = helper.cycle(number)


    def numberInCycle: Int = helper.numberInCycle(number)


    override def character: yearCompanion.Character = (isLeap, kind)


    // KH 8:7,8
    def kind: Year.Kind = {
      // XXX move to helper
      val daysOverShort = lengthInDays - (if (isLeap) 383 else 353)

      daysOverShort match {
        case 0 => Year.Short
        case 1 => Year.Regular
        case 2 => Year.Full
        case _ => throw new IllegalArgumentException("Impossible year length " + lengthInDays + " for " + this)
      }
    }
  }


  object Year extends YearCompanion {

    type Character = (Boolean, Year.Kind)


    override def apply(number: Int): Year = new Year(number)


    protected override def areYearsPositive: Boolean = true


    sealed trait Kind
    case object Short   extends Kind
    case object Regular extends Kind
    case object Full    extends Kind


    protected override def characters: Seq[yearCompanion.Character] =
      for (isLeap <- Seq(true, false); kind <- Seq(Year.Short, Year.Regular, Year.Full)) yield (isLeap, kind)


    protected override def namesAndLengths(character: yearCompanion.Character): List[(Month.Name, Int)] = character match { case (isLeap: Boolean, kind: Year.Kind) =>
      List(
        (Month.Tishrei, 30),
        (Month.Marheshvan, if (kind == Year.Full) 30 else 29),
        (Month.Kislev, if (kind == Year.Short) 29 else 30),
        (Month.Teves, 29),
        (Month.Shvat, 30)
      ) ++
        (if (!isLeap) List((Month.Adar, 29)) else List((Month.AdarI, 30), (Month.AdarII, 30))) ++
        List(
          (Month.Nisan, 30),
          (Month.Iyar, 29),
          (Month.Sivan, 30),
          (Month.Tammuz, 29),
          (Month.Av, 30),
          (Month.Elul, 29)
        )
    }


    private val adu: Set[Day.Name] = Set(Day.Rishon, Day.Rvii, Day.Shishi)


    private def isAdu(dayName: Day.Name) = adu.contains(dayName)
  }


  final class Month(number: Int) extends MonthBase(number) {

    def newMoon: Moment = Month.FirstNewMoon + Month.MeanLunarPeriod*(number-1)
  }


  object Month extends MonthCompanion {

    override def apply(number: Int): Month = new Month(number)


    sealed class Name(name: String) {

      final override def toString: String = name
    }

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


    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3)
    private val MeanLunarPeriod = Day(30).time(12, 793)


    // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2):
    // BeHaRaD: 5 hours 204 parts at night of the second day (KH 6:8)
    private val FirstNewMoon = Day(2).nightTime(5, 204)
  }


  final class Day(number: Int) extends DayBase(number) {

    def nightTime(hours: Int, parts: Int): Moment = time(Time.nightTime(hours, parts))


    def dayTime(hours: Int, parts: Int): Moment = time(Time.dayTime(hours, parts))
  }


  object Day extends DayCompanion {

    sealed class Name(name: String) {

      final override def toString: String = name
    }

    case object Rishon   extends Name("Rishon")
    case object Sheni    extends Name("Sheni")
    case object Shlishi  extends Name("Shlishi")
    case object Rvii     extends Name("Rvii")
    case object Chamishi extends Name("Chamishi")
    case object Shishi   extends Name("Shishi")
    case object Shabbos  extends Name("Shabbos")


    def names: Seq[dayCompanion.Name] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)


    override def apply(number: Int): Day = new Day(number)
  }


  final class Moment(days: Int, time: Time) extends MomentBase(days, time)


  object Moment extends MomentCompanion {

    override def apply(days: Int, time: Time): Moment = new Moment(days, time)
  }


  final class Time(hours: Int, parts: Int) extends TimeBase(hours, parts)


  object Time extends TimeCompanion {

    override def apply(hours: Int, parts: Int) = new Time(hours, parts)


    def nightTime(hours: Int, parts: Int) = {
      require(hours < Helper.hoursPerHalfDay)
      Time(hours, parts)
    }


    def dayTime(hours: Int, parts: Int) = {
      require(hours < Helper.hoursPerHalfDay)
      Time(hours + Helper.hoursPerHalfDay, parts)
    }
  }
}
