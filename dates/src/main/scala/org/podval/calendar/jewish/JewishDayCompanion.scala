package org.podval.calendar.jewish

import org.podval.calendar.dates.{Calendar, DayCompanion}
import org.podval.calendar.metadata.WithNames

abstract class JewishDayCompanion extends DayCompanion[Jewish] {
  final val Name: JewishDayCompanion.type = JewishDayCompanion

  final type Name = JewishDayCompanion.JewishDayName // TODO push into DayCompanion

  final override def names: Seq[Name] = JewishDayCompanion.values

  final override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekJewish
}

object JewishDayCompanion extends WithNames {
  sealed trait JewishDayName extends KeyBase

  override type Key = JewishDayName

  case object Rishon extends JewishDayName
  case object Sheni extends JewishDayName
  case object Shlishi extends JewishDayName
  case object Rvii extends JewishDayName
  case object Chamishi extends JewishDayName
  case object Shishi extends JewishDayName
  case object Shabbos extends JewishDayName

  override val values: Seq[JewishDayName] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)

  override def resourceName: String = "JewishDay"
}
