package org.podval.calendar.jewish

import org.podval.calendar.dates.{Calendar, DayCompanion}
import org.podval.judaica.metadata.{NamedCompanion, Named, Names}

abstract class JewishDayCompanion extends DayCompanion[Jewish] {
  final override val Name: JewishDayCompanion.type = JewishDayCompanion

  // TODO push into DayCompanion?
  final override def names: Seq[Name] = JewishDayCompanion.values

  final override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekJewish
}

object JewishDayCompanion extends NamedCompanion {
  sealed trait Key extends Named {
    final override def names: Names = toNames(this)
  }

  case object Rishon extends Key
  case object Sheni extends Key
  case object Shlishi extends Key
  case object Rvii extends Key
  case object Chamishi extends Key
  case object Shishi extends Key
  case object Shabbos extends Key

  override val values: Seq[Key] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)

  protected override def resourceName: String = "JewishDay"
}
