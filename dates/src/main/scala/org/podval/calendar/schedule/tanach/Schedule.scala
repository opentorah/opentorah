package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Custom, Haftarah}
import org.podval.calendar.jewish.Jewish.Day

final case class Schedule(
  from: Day,
  to: Day,
  inHolyLand: Boolean,
  days: Map[Day, Schedule.DaySchedule]
)

object Schedule {
  final case class DaySchedule(
    day: Day,
    morning: Option[Reading],
    afternoon: Option[Reading]
  )

  final case class Reading(
    aliyot: Custom.Of[Seq[ChumashSpan.BookSpan]],
    maftir: Option[ChumashSpan.BookSpan],
    haftarah: Option[Haftarah]
  )
}
