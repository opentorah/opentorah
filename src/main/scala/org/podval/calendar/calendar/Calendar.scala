package org.podval.calendar.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.time.{TimeInterval, TimeNumberSystem}

trait Calendar[C <: Calendar[C]] extends TimeNumberSystem[C] { this: C =>

  type Year <: YearBase[C]

  def createYear(number: Int): C#Year

  type YearCharacter

  type Month <: MonthBase[C]

  type MonthName

  def createMonth(number: Int): C#Month

  type Day <: DayBase[C]

  def createDay(number: Int): C#Day

  // TODO make this a Enum - and use its `values()` method in DayCompanion.name
  //   (which will then become `final`)?
  type DayName

  type Moment <: MomentBase[C]

  final override type Point = Moment

  def createMoment(raw: RawNumber): C#Moment

  final override def createPoint(raw: RawNumber): C#Point = createMoment(raw)

  final override type Interval = TimeInterval[C]

  final override def createInterval(raw: RawNumber): Interval =
    new TimeInterval[C](raw) { this: C#Interval =>
      final override def numberSystem: C = Calendar.this
    }

  val Year: YearCompanion[C]

  // TODO split out
  final class MonthNameAndLength(val name: C#MonthName, val length: Int)

  final def createMonthNameAndLength(name: C#MonthName, length: Int):
  C#MonthNameAndLength =
    new MonthNameAndLength(name, length)

  // TODO split out
  final class MonthDescriptor(val name: C#MonthName, val length: Int, val daysBefore: Int)

  final def createMonthDescriptor(name: C#MonthName, length: Int, daysBefore: Int):
  C#MonthDescriptor =
    new MonthDescriptor(name, length, daysBefore)

  val Month: MonthCompanion[C]

  val Day: DayCompanion[C]

  val Moment: MomentCompanion[C]

  final val moment: C#Moment = createMoment(false, List(0))

  final val interval: C#Interval = createInterval(false, List(0))

  // TODO using Day.daysPerWeek will probably break initialization too...
  final val week: C#Interval = interval.days(7)
}
