package org.podval.calendar.jewish

import org.podval.calendar.dates.{Calendar, CalendarMember}
import org.podval.calendar.numbers.{Digits, PointCompanion}

class Jewish private() extends Calendar[Jewish] {

  trait JewishCalendarMember extends CalendarMember[Jewish] {
    final override def calendar: Jewish = Jewish.this
  }

  final override type Year = JewishYear

  final override def createYear(number: Int): Year =
    new JewishYear(number) with JewishCalendarMember

  final override type YearCharacter = (Boolean, Year.Kind)

  final override object Year extends JewishYearCompanion with JewishCalendarMember

  final override type Month = JewishMonth

  final override def createMonth(number: Int): Month =
    new JewishMonth(number) with JewishCalendarMember

  final override type MonthName = Month.Name

  final override object Month extends JewishMonthCompanion with JewishCalendarMember

  final override type Day = JewishDay

  final override def createDay(number: Int): Day =
    new JewishDay(number) with JewishCalendarMember

  final override type DayName = Day.Name

  final override object Day extends JewishDayCompanion with JewishCalendarMember

  final override type Point = JewishMoment

  final override object Point extends PointCompanion[Jewish] {
    override def numbers: Jewish = Jewish.this
    override def apply(digits: Int*): Point = new Digits(digits) with JewishMoment {
      final override def companion: PointCompanion[Jewish] = Point
    }
  }
}


object Jewish extends Jewish
