package org.podval.calendar.schedule.tanach

import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.calendar.schedule.tanach.SpecialDay.{FestivalOrIntermediate, ShabbosBereishis}
import org.podval.judaica.metadata.{Language, LanguageSpec}
import org.podval.judaica.metadata.tanach.{Custom, Parsha}
import org.podval.judaica.metadata.tanach.BookSpan.ProphetSpan
import org.podval.judaica.metadata.Util

final case class Schedule(
  from: Day,
  to: Day,
  inHolyLand: Boolean,
  days: Map[Day, Schedule.DaySchedule]
)

object Schedule {

  final case class DaySchedule private(
     day: Day,
     morning: Option[Reading],
     afternoon: Option[Reading]
  )

  private final case class YearData(
    year: Year,
    shabbosBereishis: Day,
    festivals: Map[Day, FestivalOrIntermediate]
  )

  private final case class ScheduleData(
    weeklyReadings: Map[Day, WeeklyReading],
    festivals: Map[Day, FestivalOrIntermediate],
    years: Seq[Year]
  )

  // TODO we need to keep some days after 'to':
  // - next Shabbos with weekly reading for the sheni/chamishi;
  // - festival etc. for tachanun...
  def apply(from: Day, to: Day, inHolyLand: Boolean): Schedule = {
    val data: ScheduleData = scheduleData(from, to, inHolyLand)

    val daysWithSpecialReadingsNotFestivals: Map[Day, SpecialDay.Date with SpecialDay.WithReading] = filter(from, to, data.years.map(year =>
      SpecialDay.daysWithSpecialReadingsNotFestivals.map(day => day.correctedDate(year) -> day).toMap))

    val pesachOnChamishi: Set[Year] = data.years.flatMap { year =>
      if (SpecialDay.Pesach.date(year).is(Day.Name.Chamishi)) Some(year) else None }.toSet

    def forDay(day: Day): DaySchedule = getReading(
      day = day,
      weeklyReading = data.weeklyReadings.get(day),
      specialDay = data.festivals.get(day).orElse(daysWithSpecialReadingsNotFestivals.get(day)),
      isPesachOnChamishi = pesachOnChamishi.contains(day.year)
    )

    val days: Seq[Day] = Util.unfoldSimple[Day](from, _ + 1, _ <= to)

    new Schedule(
      from = from,
      to = to,
      inHolyLand = inHolyLand,
      days = days.map(day => day -> forDay(day)).toMap
    )
  }

  private final def getReading(
    day: Day,
    weeklyReading: Option[WeeklyReading],
    specialDay: Option[SpecialDay.WithReading],
    isPesachOnChamishi: Boolean
  ): DaySchedule = {
    val isShabbos: Boolean = day.isShabbos
    val isRoshChodesh: Boolean = day.isRoshChodesh

    // TODO we need the next weekly reading to determine the sheni/chamishi one
    val afternoonSpecial: Option[Reading] = specialDay flatMap { specialDay => specialDay.getAfternoonReading }

    DaySchedule(
      day,
      morning =
        specialDay.map(_.getReading(isShabbos, isRoshChodesh, weeklyReading, isPesachOnChamishi))
          .orElse(weeklyReading.map(_.getReading)),
      afternoon = afternoonSpecial
    )
  }

  private def scheduleData(from: Day, to: Day, inHolyLand: Boolean): ScheduleData = {
    val fromYear: Year = if (ShabbosBereishis.date(from.year) <= from) from.year else from.year-1
    val toYear: Year = if (to < ShabbosBereishis.date(to.year)) to.year else to.year+1
    val years: Seq[Year] = Util.unfoldSimple[Year](fromYear, _ + 1, _ <= toYear)

    val yearsData: Seq[YearData] = years.map { year =>
      YearData(
        year = year,
        shabbosBereishis = ShabbosBereishis.date(year),
        festivals = SpecialDay.festivals(inHolyLand).map { specialDay =>  specialDay.date(year) -> specialDay }.toMap
      )
    }

    val weeklyReadings: Seq[Map[Day, WeeklyReading]] = (yearsData zip yearsData.tail).map { case (current, next) =>
      WeeklyReading.getCycle(
        year = current.year,
        fromShabbosBereishis = current.shabbosBereishis,
        toShabbosBereishis = next.shabbosBereishis,
        festivals = current.festivals.keySet ++ next.festivals.keySet
      ).toMap
    }

    ScheduleData(
      weeklyReadings = filter(from, to, weeklyReadings),
      festivals = filter(from, to, yearsData.map(_.festivals)),
      years = years
    )
  }

  private def filter[T](from: Day, to: Day, data: Seq[Map[Day, T]]): Map[Day, T] = {
    val result = data.head.filterKeys(from <= _) +: data.tail.init :+ data.last.filterKeys(_ <= to)
    result.flatten.toMap
  }

  def forYear(year: Year, inHolyLand: Boolean): Schedule = Schedule(year.firstDay, year.lastDay, inHolyLand)

  // Used by tests only
  def weeklyReadingsForYear(year: Year, inHolyLand: Boolean): Map[Day, WeeklyReading]  =
    scheduleData(year.firstDay, year.lastDay, inHolyLand).weeklyReadings

  def printHaftarahList(custom: Custom, spec: LanguageSpec, full: Boolean): Unit = {
    println(custom.toString(spec))
    for (parsha <- Parsha.values) {
      val haftarah: Haftarah = Haftarah.forParsha(parsha)
      val customEffective: Custom = Custom.find(haftarah.customs, custom)
      val spans: Seq[ProphetSpan.BookSpan] = haftarah.customs(customEffective)
      val result: String = ProphetSpan.toString(spans, spec)

      if (customEffective == custom) {
        println(parsha.toString(spec) + ": " + result)
      } else if (full) {
        println(parsha.toString(spec) + " [" + customEffective.toString(spec)  + "]" + ": " + result)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(Parsha.Mattos.getDaysCombined(Custom.Ashkenaz).toString(Language.Hebrew.toSpec))
    println()
    printHaftarahList(Custom.Shami, Language.Hebrew.toSpec, full = false)
    println()
//    println(SpecialReading.SheminiAtzeres.shabbosAliyot.get.toString(Language.English.toSpec))
    println()
    println(SpecialDay.SheminiAtzeres.getReading(false).maftir.get.toString(Language.English.toSpec))
    println()

    val year = Year(5779)
    val day = year.month(Month.Name.Marheshvan).day(25)
    val schedule: Schedule = forYear(year, inHolyLand = false)
    val daySchedule = schedule.days(day)
    println(daySchedule)
  }
}
