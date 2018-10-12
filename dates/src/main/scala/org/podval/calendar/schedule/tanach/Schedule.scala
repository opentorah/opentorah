package org.podval.calendar.schedule.tanach

import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.calendar.jewish.SpecialDay
import org.podval.calendar.jewish.SpecialDay.{FestivalOrIntermediate, RabbinicFestival, Fast, shabbosBereishis}
import org.podval.judaica.metadata.{Language, LanguageSpec}
import org.podval.judaica.metadata.tanach.{Custom, Haftarah, Parsha, SpecialReading}
import org.podval.judaica.metadata.tanach.BookSpan.ProphetSpan
import org.podval.judaica.metadata.Util

final case class Schedule(
  from: Day,
  to: Day,
  inHolyLand: Boolean,
  days: Map[Day, DaySchedule],
  weeklyReadings: Map[Day, WeeklyReading] // TODO used in ScheduleTest only; eliminate
)

object Schedule {

  private final case class YearData(
    year: Year,
    shabbosBereishis: Day,
    festivals: Map[Day, FestivalOrIntermediate],
    rabbinicFestivals: Map[Day, RabbinicFestival],
    fasts: Map[Day, Fast]
  )

  // TODO we need to keep some days after 'to':
  // - next Shabbos with weekly reading for the sheni/chamishi;
  // - festival etc. for tachanun...
  def apply(from: Day, to: Day, inHolyLand: Boolean): Schedule = {
    def filter[T](data: Seq[Map[Day, T]]): Map[Day, T] = {
      val result = data.head.filterKeys(from <= _) +: data.tail.init :+ data.last.filterKeys(_ <= to)
      result.flatten.toMap
    }

    val fromYear: Year = if (shabbosBereishis(from.year) <= from) from.year else from.year-1
    val toYear: Year = if (to < shabbosBereishis(to.year)) to.year else to.year+1
    val years: Seq[Year] = Util.unfoldSimple[Year](fromYear, _ + 1, _ <= toYear)

    val yearsData: Seq[YearData] = years.map { year =>
      YearData(
        year = year,
        shabbosBereishis = shabbosBereishis(year),
        festivals = SpecialDay.festivals(inHolyLand).map { specialDay =>  specialDay(year) -> specialDay }.toMap,
        rabbinicFestivals = SpecialDay.rabbinicFestivals.map { specialDay =>  specialDay(year) -> specialDay }.toMap,
        fasts = SpecialDay.fasts.map { specialDay =>  specialDay.corrected(year) -> specialDay }.toMap
      )
    }

    val cycles: Seq[Map[Day, WeeklyReading]] = (yearsData zip yearsData.tail).map { case (current, next) =>
      WeeklyReading.getCycle(
        year = current.year,
        fromShabbosBereishis = current.shabbosBereishis,
        toShabbosBereishis = next.shabbosBereishis,
        festivals = current.festivals.keySet ++ next.festivals.keySet
      ).toMap
    }

    val weeklyReadings: Map[Day, WeeklyReading] = filter(cycles)
    val festivals: Map[Day, FestivalOrIntermediate] = filter(yearsData.map(_.festivals))
    val rabbinicFestivals: Map[Day, RabbinicFestival] = filter(yearsData.map(_.rabbinicFestivals))
    val fasts: Map[Day, Fast] = filter(yearsData.map(_.fasts))

    def forDay(day: Day): DaySchedule = {
      require(from <= day && day <= to)

      DaySchedule(
        day = day,
        weeklyReading = weeklyReadings.get(day),
        festival = festivals.get(day),
        rabbinicFestival = rabbinicFestivals.get(day),
        fast = fasts.get(day)
      )
    }

    new Schedule(
      from = from,
      to = to,
      inHolyLand = inHolyLand,
      days = Util.unfoldSimple[Day](from, _ + 1, _ <= to).map(forDay).map(s => s.day -> s).toMap,
      weeklyReadings = weeklyReadings
    )
  }

  def forYear(year: Year, inHolyLand: Boolean): Schedule = Schedule(year.firstDay, year.lastDay, inHolyLand)

  def printHaftarahList(custom: Custom, spec: LanguageSpec, full: Boolean): Unit = {
    println(custom.toString(spec))
    for (parsha <- Parsha.values) {
      val haftarah: Haftarah = parsha.haftarah
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
    printHaftarahList(Custom.Shami, Language.Hebrew.toSpec, full = false)
    println(SpecialReading.SheminiAtzeres.shabbosAliyot.get.toString(Language.English.toSpec))
    println(SpecialReading.SheminiAtzeresMaftir.maftir.get.toString(Language.English.toSpec))

    val year = Year(5779)
    val day = year.month(Month.Name.Marheshvan).day(25)
    val schedule: Schedule = forYear(year, inHolyLand = false)
    val daySchedule = schedule.days(day)
    println()
    println(daySchedule)
  }
}
