package org.podval.calendar.jewish

import org.podval.calendar.util.Named

sealed class JewishDayName(name: String) extends Named(name)

object JewishDayName {
  case object Rishon extends JewishDayName("Rishon")
  case object Sheni extends JewishDayName("Sheni")
  case object Shlishi extends JewishDayName("Shlishi")
  case object Rvii extends JewishDayName("Rvii")
  case object Chamishi extends JewishDayName("Chamishi")
  case object Shishi extends JewishDayName("Shishi")
  case object Shabbos extends JewishDayName("Shabbos")

  val values: Seq[JewishDayName] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)
}
