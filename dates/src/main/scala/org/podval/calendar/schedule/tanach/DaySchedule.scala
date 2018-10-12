package org.podval.calendar.schedule.tanach

import org.podval.calendar.jewish.Jewish.Day
import org.podval.calendar.jewish.SpecialDay
import org.podval.calendar.jewish.SpecialDay.{Fast, FestivalOrIntermediate, RabbinicFestival}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Custom, Haftarah}

final case class DaySchedule(
  day: Day,
  morning: Option[DaySchedule.Reading],
  afternoon: Option[DaySchedule.Reading]
)

object DaySchedule {

  final case class Reading(
    aliyot: Custom.Of[Seq[ChumashSpan.BookSpan]],
    maftir: Option[ChumashSpan.BookSpan],
    haftarah: Option[Haftarah]
  )

  final def apply(
    day: Day,
    weeklyReading: Option[WeeklyReading],
    festival: Option[FestivalOrIntermediate],
    rabbinicFestival: Option[RabbinicFestival],
    fast: Option[Fast]
  ): DaySchedule = {
    val hasWeeklyReading: Boolean = weeklyReading.isDefined
    val isFestival: Boolean = festival.isDefined
    val isRabbinicFestival: Boolean = rabbinicFestival.isDefined
    val isFast: Boolean = fast.isDefined

    val isShabbos: Boolean = day.isShabbos
    val isRoshChodesh: Boolean = day.isRoshChodesh

    if (hasWeeklyReading) require(isShabbos)
    if (isShabbos && !isFestival) require(weeklyReading.nonEmpty)
    if (isFestival) require(!isRabbinicFestival)
    if (isFestival || isRabbinicFestival || isShabbos || isRoshChodesh) require(!isFast)

    DaySchedule(
      day,
      morning = morning(weeklyReading, festival, rabbinicFestival, fast),
      afternoon = afternoon(day, festival, fast)
    )
  }

  private def morning(
    weeklyReading: Option[WeeklyReading],
    festival: Option[FestivalOrIntermediate],
    rabbinicFestival: Option[RabbinicFestival],
    fast: Option[Fast]
  ): Option[Reading] = {
    // TODO Shabbos Chanukkah *adds* to the weeklyReading!
    // TODO:
//    festival.map(festivalMorning)
//      .orElse(rabbinicFestival.map(rabbinicFestivalMorning))
//      .orElse(fast.map(fastMorning))
    None
      .orElse(weeklyReading.map(_.getReading))
  }

  private def festivalMorning(festival: FestivalOrIntermediate): Reading = ???

  private def rabbinicFestivalMorning(rabbinicFestival: RabbinicFestival): Reading = ???

  private def fastMorning(fast: Fast): Reading = ???

  private def afternoon(
    day: Day,
    festival: Option[FestivalOrIntermediate],
    fast: Option[Fast]
  ): Option[Reading] = {
    val isShabbos: Boolean = day.isShabbos
    val isYomKippur: Boolean = festival.contains(SpecialDay.YomKippur)
    val isTishaBeAv: Boolean = fast.contains(SpecialDay.TishaBeAv)

    // TODO
    None
  }
}
