package org.podval.calendar.jewish

import org.podval.calendar.dates.{Calendar, DayCompanion}
import org.podval.calendar.metadata.{MetadataParser, Names, WithNames}

abstract class JewishDayCompanion extends DayCompanion[Jewish] {
  final val Name: JewishDayCompanion.type = JewishDayCompanion

  final type Name = Name.Name // TODO push into DayCompanion

  final override def names: Seq[Name] = JewishDayCompanion.values

  final override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekJewish
}


object JewishDayCompanion {
  sealed trait Name extends WithNames[Name] {
    def toNames: Map[Name, Names] = day2names
  }

  case object Rishon extends Name
  case object Sheni extends Name
  case object Shlishi extends Name
  case object Rvii extends Name
  case object Chamishi extends Name
  case object Shishi extends Name
  case object Shabbos extends Name

  val values: Seq[Name] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)

  private val day2names: Map[Name, Names] = MetadataParser.loadNames(this, "JewishDay", values)
}
