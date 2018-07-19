package org.podval.calendar.dates

import org.podval.calendar.gregorian.Gregorian
import org.podval.calendar.jewish.Jewish
import org.podval.calendar.numbers.{VectorCompanion, PointCompanion}
import org.podval.calendar.time.TimeNumberSystem.hoursPerDay
import org.podval.calendar.time.{TimeVectorBase, TimeNumberSystem}

trait Calendar[C <: Calendar[C]] extends TimeNumberSystem[C] { this: C =>

  type Year <: YearBase[C]

  def createYear(number: Int): Year

  type YearCharacter

  val Year: YearCompanion[C]

  type Month <: MonthBase[C]

  type MonthName

  def createMonth(number: Int): Month

  type MonthNameAndLength = MonthNameAndLengthBase[C]

  type MonthDescriptor = MonthDescriptorBase[C]

  val Month: MonthCompanion[C]

  type Day <: DayBase[C]

  def createDay(number: Int): Day

  type DayName

  val Day: DayCompanion[C]

  final type Moment = Point

  override type Point <: MomentBase[C]

  final val Moment: PointCompanion[C] = Point

  final override type Vector = TimeVectorBase[C]

  // TODO rename TimeSpan?
  final type TimeVector = Vector

  final override def createVector(digits: Seq[Int]): Vector =
    new TimeVectorBase[C](digits) { this: Vector =>
      final override def numberSystem: C = Calendar.this
    }

  final override object Vector extends VectorCompanion[C] {
    override def numberSystem: C = Calendar.this
  }

  final val TimeVector = Vector
}


object Calendar {
  final val daysPerWeek: Int = 7

  // It seems that first day of the first year was Sunday; molad - BaHaRad.
  // Second year - Friday; molad - 8 in the morning.
  final val firstDayNumberInWeekJewish: Int = 1

  final val epoch: Int = 1373429

  final val firstDayNumberInWeekGregorian: Int =
    (((firstDayNumberInWeekJewish - 1) + epoch % daysPerWeek) % daysPerWeek) + 1

  //  Jewish  :   6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0  1  2  3  4  5  6
  //  Georgian:  |0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0
  private final val dayStartHoursJewish = 18

  private final val dayStartHoursGregorian: Int = hoursPerDay - dayStartHoursJewish

  final def toJewish(moment: Gregorian.Moment): Jewish.Moment = {
    val hours = moment.hours

    val (newDay, newHours) =
      if (hours >= dayStartHoursJewish)
        (moment.day.next, hours - dayStartHoursJewish) else
        (moment.day     , hours + dayStartHoursGregorian)

    toJewish(newDay).toMoment.hours(newHours).parts(moment.parts)
  }

  final def fromJewish(moment: Jewish.Moment): Gregorian.Moment = {
    val hours = moment.hours

    val (newDay, newHours) =
      if (hours < dayStartHoursGregorian)
        (moment.day.prev, hours + dayStartHoursJewish) else
        (moment.day     , hours - dayStartHoursGregorian)

    fromJewish(newDay).toMoment.hours(newHours).parts(moment.parts)
  }

  final def fromJewish(day: Jewish   .Day): Gregorian.Day = Gregorian.Day(day.number - epoch)

  final def toJewish  (day: Gregorian.Day): Jewish   .Day = Jewish   .Day(day.number + epoch)
}
