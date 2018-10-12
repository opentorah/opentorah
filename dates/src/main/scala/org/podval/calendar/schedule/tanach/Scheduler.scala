package org.podval.calendar.schedule.tanach

import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.calendar.jewish.SpecialDay
import SpecialDay.ShabbosBereishis
import org.podval.judaica.metadata.{Language, LanguageSpec}
import org.podval.judaica.metadata.tanach.{Custom, Haftarah, Parsha, SpecialReading}
import org.podval.judaica.metadata.tanach.BookSpan.ProphetSpan
import org.podval.judaica.metadata.Util



final case class Scheduler private(
  from: Day,
  to: Day,
  inHolyLand: Boolean,
  weeklyReadings: Map[Day, WeeklyReading],
  festivals: Map[Day, SpecialDay]
  // TODO add other special days: Chanukkah, Purim, Fasts...
) {
  def schedule: Schedule = Schedule(
    from = from,
    to = to,
    inHolyLand = inHolyLand,
    days = Util.unfoldSimple[Day](from, _ + 1, _ <= to).map(forDay).map(s => s.day -> s).toMap
  )

  def forDay(day: Day): Schedule.DaySchedule = {
    require(from <= day && day <= to)

    val festival: Option[SpecialDay] = festivals.get(day)
    val isShabbos: Boolean = day.isShabbos
    val weeklyReading: Option[WeeklyReading] = weeklyReadings.get(day)

    require(weeklyReading.isEmpty || isShabbos)
    require(weeklyReading.nonEmpty || (!isShabbos || festival.nonEmpty))

    //    val isRoshChodesh: Boolean = ???

    val morning: Option[Schedule.Reading] = if (weeklyReading.nonEmpty) Some(weeklyReading.get.getReading) else None

    Schedule.DaySchedule(
      day,
      morning,
      None
    )
  }
}

object Scheduler {

  def forYear(year: Year, inHolyLand: Boolean): Scheduler = getScheduler(year.firstDay, year.lastDay, inHolyLand)

  private final case class YearData(year: Year, shabbosBereishis: Day, festivals: Map[Day, SpecialDay])

  def getScheduler(from: Day, to: Day, inHolyLand: Boolean): Scheduler = {
    def filter[T](data: Seq[Map[Day, T]]): Map[Day, T] = {
      val result = data.head.filterKeys(from <= _) +: data.tail.init :+ data.last.filterKeys(_ <= to)
      result.flatten.toMap
    }

    val festivalDays: Set[SpecialDay] = SpecialDay.festivals(inHolyLand)
    val fromYear: Year = if (ShabbosBereishis(from.year) <= from) from.year else from.year-1
    val toYear: Year = if (to < ShabbosBereishis(to.year)) to.year else to.year+1
    val years: Seq[Year] = Util.unfoldSimple[Year](fromYear, _ + 1, _ <= toYear)

    val yearsData: Seq[YearData] = years.map { year =>
      YearData(
        year = year,
        shabbosBereishis = ShabbosBereishis(year),
        festivals = festivalDays.map { specialDay =>  specialDay(year) -> specialDay }.toMap
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

    new Scheduler(
      from = from,
      to = to,
      inHolyLand = inHolyLand,
      weeklyReadings = filter(weeklyReadings),
      festivals = filter(yearsData.map(_.festivals))
    )
  }

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
    val schedule: Schedule = forYear(year, inHolyLand = false).schedule
    val daySchedule = schedule.days(day)
    println()
    println(daySchedule)
  }
}
