package org.podval.calendar.jewish

import org.podval.calendar.calendar._
import org.podval.calendar.numbers.NumberSystem.RawNumber

// TODO add a check that length of the year and total length of the months are the same
class Jewish private() extends Calendar[Jewish] {

  trait JewishCalendarMember extends CalendarMember[Jewish] {
    final override def calendar: Jewish = Jewish.this
  }

  final override type Year = JewishYear

  final override def createYear(number: Int): Jewish.Year =
    new JewishYear(number) with JewishCalendarMember

  final override type YearCharacter = (Boolean, Year.Kind)

  final override object Year extends JewishYearCompanion with JewishCalendarMember

  final override type Month = JewishMonth

  final override def createMonth(number: Int): Jewish.Month =
    new JewishMonth(number) with JewishCalendarMember

  // TODO eliminate
  final override type MonthName = Month.Name

  final override object Month extends JewishMonthCompanion with JewishCalendarMember

  final override type Day = JewishDay

  final override def createDay(number: Int): Jewish.Day =
    new JewishDay(number) with JewishCalendarMember

  final override type DayName = Day.Name

  final override object Day extends JewishDayCompanion with JewishCalendarMember

  final override type Moment = JewishMoment

  final override def createMoment(raw: RawNumber): Jewish.Moment =
    new JewishMoment(raw) with JewishCalendarMember {
      final override def numberSystem:  Jewish = Jewish.this
    }

  final override object Moment extends JewishMomentCompanion with JewishCalendarMember
}


object Jewish extends Jewish
