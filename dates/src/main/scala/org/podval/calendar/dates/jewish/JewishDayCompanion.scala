package org.podval.calendar.dates.jewish

import org.podval.calendar.dates.calendar.{Calendar, DayCompanion}
import org.podval.calendar.util.Named

abstract class JewishDayCompanion extends DayCompanion[Jewish] {
  final type Name = JewishDayCompanion.Name

  final val Name: JewishDayCompanion.Name.type = JewishDayCompanion.Name

  final override def names: Seq[Name] = Name.values

  final override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekJewish
}


object JewishDayCompanion {
  sealed class Name(name: String) extends Named(name)

  object Name {
    case object Rishon extends Name("Rishon")
    case object Sheni extends Name("Sheni")
    case object Shlishi extends Name("Shlishi")
    case object Rvii extends Name("Rvii")
    case object Chamishi extends Name("Chamishi")
    case object Shishi extends Name("Shishi")
    case object Shabbos extends Name("Shabbos")

    val values: Seq[Name] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)
  }
}
