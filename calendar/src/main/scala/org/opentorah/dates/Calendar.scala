package org.opentorah.dates

import org.opentorah.metadata.LanguageSpec
import org.opentorah.numbers.{Digits, VectorCompanion}
import org.opentorah.times.{TimeVectorBase, Times}

trait Calendar[C <: Calendar[C]] extends Times[C] { this: C =>

  type Year <: YearBase[C]

  type YearCharacter

  final val cacheYears: Boolean = true

  val Year: YearCompanion[C]

  type Month <: MonthBase[C]

  final type MonthName = Month.Name

  type MonthNameAndLength = MonthNameAndLengthBase[C]

  type MonthDescriptor = MonthDescriptorBase[C]

  val Month: MonthCompanion[C]

  type Day <: DayBase[C]

  final type DayName = Day.Name

  val Day: DayCompanion[C]

  final type Moment = Point

  override type Point <: MomentBase[C]

  override val Point: MomentCompanion[C]

  final lazy val Moment: MomentCompanion[C] = Point

  final override type Vector = TimeVectorBase[C]

  final type TimeVector = Vector

  final override lazy val Vector: VectorCompanion[C] = new VectorCompanion[C](Calendar.this) {
    protected override def newNumber(digits: Digits): Vector =
      new TimeVectorBase[C](Calendar.this, digits) {
        final override def companion: VectorCompanion[C] = Vector
      }
  }

  final val TimeVector = Vector

  def toString(number: Int)(implicit spec: LanguageSpec): String
}


object Calendar {
  final val daysPerWeek: Int = 7

  // It seems that first day of the first year was Sunday; molad - BaHaRad.
  // Second year - Friday; molad - 8 in the morning.
  final val firstDayNumberInWeekJewish: Int = 1

  final val epoch: Int = 1373429

  final val firstDayNumberInWeekGregorian: Int =
    (((firstDayNumberInWeekJewish - 1) + epoch % daysPerWeek) % daysPerWeek) + 1
}
