package org.podval.calendar.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.time.{TimeInterval, TimeNumberSystem}

trait Calendar[C <: Calendar[C]] extends TimeNumberSystem[C] { this: C =>

  type Year <: YearBase[C]

  def createYear(number: Int): C#Year

  // TODO scope inside YearCompanion?
  type YearCharacter

  val Year: YearCompanion[C]

  type Month <: MonthBase[C]

  // TODO scope inside MonthCompanion?
  type MonthName

  def createMonth(number: Int): C#Month

  // TODO scope inside MonthCompanion?
  type MonthNameAndLength = MonthNameAndLengthBase[C]

  // TODO scope inside MonthCompanion?
  type MonthDescriptor = MonthDescriptorBase[C]

  val Month: MonthCompanion[C]

  type Day <: DayBase[C]

  def createDay(number: Int): C#Day

  // TODO scope inside DayCompanion?
  // TODO make this a Enum - and use its `values()` method in DayCompanion.name
  //   (which will then become `final`); also define val Name: X...
  type DayName

  val Day: DayCompanion[C]

  type Moment <: MomentBase[C]

  final override type Point = Moment

  def createMoment(raw: RawNumber): C#Moment

  final override def createPoint(raw: RawNumber): C#Point = createMoment(raw)

  final override type Interval = TimeInterval[C]

  final override def createInterval(raw: RawNumber): Interval =
    new TimeInterval[C](raw) { this: C#Interval =>
      final override def numberSystem: C = Calendar.this
    }

  val Moment: MomentCompanion[C]

  final val moment: C#Moment = createMoment(false, List(0))

  final val interval: C#Interval = createInterval(false, List(0))

  // TODO using Day.daysPerWeek will probably break initialization too...
  final val week: C#Interval = interval.days(7)
}
