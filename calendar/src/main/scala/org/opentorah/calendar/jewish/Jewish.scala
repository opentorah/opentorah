package org.opentorah.calendar.jewish

import org.opentorah.metadata.LanguageSpec
import org.opentorah.dates.Calendar

class Jewish private() extends Calendar[Jewish] {

  final override type Year = JewishYear

  final override type YearCharacter = (Boolean, Year.Kind)

  final override lazy val Year = new JewishYearCompanion(Jewish.this) {
    protected override def newYear(number: Int): Year =
      new JewishYear(Jewish.this, number)
  }

  final override type Month = JewishMonth

  final override lazy val Month = new JewishMonthCompanion(Jewish.this) {
    private[opentorah] override def apply(yearOpt: Option[Year], number: Int): Month =
      new JewishMonth(Jewish.this, yearOpt, number)
  }

  final override type Day = JewishDay

  final override lazy val Day = new JewishDayCompanion(Jewish.this) {
    private[opentorah] override def apply(monthOpt: Option[Month], number: Int): Day =
      new JewishDay(Jewish.this, monthOpt, number)
  }

  final override type Point = JewishMoment

  final override type PointCompanionType = JewishMomentCompanion

  final override lazy val Point = new JewishMomentCompanion(Jewish.this) {
    protected override def newNumber(digits: Seq[Int]): Point =
      new JewishMoment(Jewish.this, digits) {
        final override def companion: PointCompanionType = Point
      }
  }

  final override def toString(number: Int)(implicit spec: LanguageSpec): String = spec.toString(number)
}


object Jewish extends Jewish
