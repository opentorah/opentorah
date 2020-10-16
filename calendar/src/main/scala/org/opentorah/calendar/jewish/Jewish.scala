package org.opentorah.calendar.jewish

import org.opentorah.metadata.LanguageSpec
import org.opentorah.dates.{Calendar, CalendarMember}

class Jewish private() extends Calendar[Jewish] {

  trait JewishCalendarMember extends CalendarMember[Jewish] {
    final override def numbers: Jewish = Jewish.this
  }

  final override type NumbersMemberType = JewishCalendarMember

  final override type Year = JewishYear

  final override type YearCharacter = (Boolean, Year.Kind)

  final override object Year extends JewishYearCompanion with JewishCalendarMember {
    protected override def newYear(number: Int): Year =
      new JewishYear(number) with JewishCalendarMember
  }

  final override type Month = JewishMonth

  final override object Month extends JewishMonthCompanion with JewishCalendarMember {
    private[opentorah] override def apply(yearOpt: Option[Year], number: Int): Month =
      new JewishMonth(yearOpt, number) with JewishCalendarMember
  }

  final override type Day = JewishDay

  final override object Day extends JewishDayCompanion with JewishCalendarMember {
    private[opentorah] override def apply(monthOpt: Option[Month], number: Int): Day =
      new JewishDay(monthOpt, number) with JewishCalendarMember
  }

  final override type Point = JewishMoment

  final override type PointCompanionType = JewishMomentCompanion

  final override object Point extends JewishMomentCompanion with NumbersMemberType {
    protected override def newNumber(digits: Seq[Int]): Point =
      new JewishMoment(digits) with NumbersMemberType {
        final override def companion: PointCompanionType = Point
      }
  }

  final override def toString(number: Int)(implicit spec: LanguageSpec): String = spec.toString(number)
}


object Jewish extends Jewish
