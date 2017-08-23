package org.podval.calendar.dates.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.dates.time.{TimeIntervalBase, TimeNumberSystem}

trait Calendar[C <: Calendar[C]] extends TimeNumberSystem[C] { this: C =>

  type Year <: YearBase[C]

  def createYear(number: Int): C#Year

  type YearCharacter

  val Year: YearCompanion[C]

  type Month <: MonthBase[C]

  type MonthName

  def createMonth(number: Int): C#Month

  type MonthNameAndLength = MonthNameAndLengthBase[C]

  type MonthDescriptor = MonthDescriptorBase[C]

  val Month: MonthCompanion[C]

  type Day <: DayBase[C]

  def createDay(number: Int): C#Day

  type DayName

  val Day: DayCompanion[C]

  type Moment <: MomentBase[C]

  final override type Point = Moment

  def createMoment(raw: RawNumber): C#Moment

  final override def createPoint(raw: RawNumber): C#Point = createMoment(raw)

  final override type Interval = TimeIntervalBase[C]

  final type TimeInterval = Interval

  final override def createInterval(raw: RawNumber): TimeInterval =
    new TimeIntervalBase[C](raw) { this: C#TimeInterval =>
      final override def numberSystem: C = Calendar.this
    }

  val Moment: MomentCompanion[C]

  final val moment: C#Moment = createMoment(false, List(0))

  final val interval: C#TimeInterval = createInterval(false, List(0))

  final val week: C#TimeInterval = interval.days(Calendar.daysPerWeek)
}


object Calendar {
  final val daysPerWeek: Int = 7
}
