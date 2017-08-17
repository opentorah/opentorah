package org.podval.calendar.jewish

sealed trait JewishYearKind

object JewishYearKind {
  case object Short extends JewishYearKind
  case object Regular extends JewishYearKind
  case object Full extends JewishYearKind

  val values: Seq[JewishYearKind] = Seq(Short, Regular, Full)
}
