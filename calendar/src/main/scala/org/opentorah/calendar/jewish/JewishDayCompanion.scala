package org.opentorah.calendar.jewish

import org.opentorah.metadata.{Named, NamedCompanion, Names}
import org.opentorah.dates.{Calendar, DayCompanion}

trait JewishDayCompanion extends DayCompanion[Jewish] {
  final override val Name: JewishDayCompanion.type = JewishDayCompanion

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
