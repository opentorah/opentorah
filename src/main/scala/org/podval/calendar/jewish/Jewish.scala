package org.podval.calendar.jewish

import org.podval.calendar.calendar._
import org.podval.calendar.numbers.NumberSystem.RawNumber

// TODO add a check that length of the year and total length of the months are the same
class Jewish private() extends Calendar[Jewish] {

  trait JewishCalendarMember extends CalendarMember[Jewish] {
    final override def calendar: Jewish = Jewish.this
  }

  final override type Year = JewishYear

  final override def createYear(number: Int): Jewish#Year =
    new JewishYear(number) with JewishCalendarMember

  final type YearKind = JewishYearKind

  // TODO stick this into the Year companion?
  final val YearKind: JewishYearKind.type = JewishYearKind

  final override type YearCharacter = (Boolean, YearKind)

  final override val Year: JewishYearCompanion =
    new JewishYearCompanion with JewishCalendarMember

  final override type Month = JewishMonth

  final override def createMonth(number: Int): Jewish#Month =
    new JewishMonth(number) with JewishCalendarMember

  final override type MonthName = JewishMonthName

  final override val Month: JewishMonthCompanion =
    new JewishMonthCompanion with JewishCalendarMember

  final override type Day = JewishDay

  final override def createDay(number: Int): Jewish#Day =
    new JewishDay(number) with JewishCalendarMember

  final override type DayName = JewishDayName

  final override val Day: JewishDayCompanion = new JewishDayCompanion with JewishCalendarMember

  final override type Moment = JewishMoment

  final override def createMoment(raw: RawNumber): Jewish#Moment =
    new JewishMoment(raw) with JewishCalendarMember {
      final override def numberSystem:  Jewish = Jewish.this
    }

  final override val Moment: JewishMomentCompanion =
    new JewishMomentCompanion with JewishCalendarMember
}


object Jewish extends Jewish
