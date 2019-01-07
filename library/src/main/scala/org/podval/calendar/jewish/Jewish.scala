package org.podval.calendar.jewish

import org.podval.calendar.dates.{Calendar, CalendarMember}
import org.podval.calendar.numbers.{Digits, PointCompanion}
import org.podval.judaica.metadata.LanguageSpec

class Jewish private() extends Calendar[Jewish] {

  trait JewishCalendarMember extends CalendarMember[Jewish] {
    final override def numbers: Jewish = Jewish.this
  }

  final override type Year = JewishYear

  final override type YearCharacter = (Boolean, Year.Kind)

  final override object Year extends JewishYearCompanion with JewishCalendarMember {
    protected override def newYear(number: Int): Year =
      new JewishYear(number) with JewishCalendarMember
  }

  final override type Month = JewishMonth

  final override object Month extends JewishMonthCompanion with JewishCalendarMember {
    protected override def apply(year: Year, number: Int): Month =
      new JewishMonth(year, number) with JewishCalendarMember
  }

  final override type Day = JewishDay

  final override object Day extends JewishDayCompanion with JewishCalendarMember {
    override def apply(number: Int): Day = new JewishDay(number) with JewishCalendarMember
  }

  final override type Point = JewishMoment

  final override object Point extends JewishMomentCompanion with JewishCalendarMember {
    override def apply(digits: Int*): Point = new Digits(digits) with JewishMoment with JewishCalendarMember {
      final override def companion: PointCompanion[Jewish] = Point
    }
  }

  final override def toString(number: Int)(implicit spec: LanguageSpec): String = spec.toString(number)
}


object Jewish extends Jewish
