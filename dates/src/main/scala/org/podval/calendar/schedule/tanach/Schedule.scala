package org.podval.calendar.schedule.tanach

import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.calendar.schedule.tanach.SpecialDay.{FestivalOrIntermediate, ShabbosBereishis}
import org.podval.judaica.metadata.{Language, LanguageSpec, Util}
import org.podval.judaica.metadata.tanach.{Aliyot, Custom, Parsha}
import org.podval.judaica.metadata.tanach.BookSpan.ProphetSpan

final case class Schedule private(
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

  def apply(from: Day, to: Day, inHolyLand: Boolean): Schedule = createBuilder(from, to, inHolyLand).build

  def apply(year: Year, inHolyLand: Boolean): Schedule = Schedule(year.firstDay, year.lastDay, inHolyLand)

  private final case class Builder private(
    from: Day,
    to: Day,
    inHolyLand: Boolean,
    weeklyReadings: Map[Day, WeeklyReading],
    festivals: Map[Day, FestivalOrIntermediate],
    years: Seq[Year],
    daysWithSpecialReadingsNotFestivals: Map[Day, SpecialDay.Date],
    specialShabboses: Map[Day, SpecialDay.SpecialShabbos],
    pesachOnChamishi: Set[Year]
  ) {
    def build: Schedule = {
      val days: Seq[Day] = Util.unfoldSimple[Day](from, _ + 1, _ <= to)

      new Schedule(
        from = from,
        to = to,
        inHolyLand = inHolyLand,
        days = days.map(day => day -> forDay(day)).toMap
      )
    }

    private def forDay(day: Day): DaySchedule = {
      val weeklyReading: Option[WeeklyReading] = weeklyReadings.get(day)
      val specialDay: Option[SpecialDay.Date] = festivals.get(day).orElse(daysWithSpecialReadingsNotFestivals.get(day))
      val specialShabbos: Option[SpecialDay.SpecialShabbos] = specialShabboses.get(day)
      val isPesachOnChamishi: Boolean = pesachOnChamishi.contains(day.year)
      val nextWeeklyReading: WeeklyReading = nextWeeklyReadingFor(day)

      val isShabbos: Boolean = day.isShabbos
      if (!isShabbos) require(weeklyReading.isEmpty && specialShabbos.isEmpty)

      val isRoshChodesh: Boolean = day.isRoshChodesh
      val isSheniOrChamishi: Boolean = Set[Day.Name](Day.Name.Sheni, Day.Name.Chamishi).contains(day.name)

      DaySchedule(
        day,
        morning = SpecialDay.getMorningReading(
          specialDay = specialDay,
          specialShabbos = specialShabbos,
          weeklyReading = weeklyReading,
          nextWeeklyReading = nextWeeklyReading,
          isShabbos = isShabbos,
          isRoshChodesh = isRoshChodesh,
          isPesachOnChamishi = isPesachOnChamishi,
          isSheniOrChamishi = isSheniOrChamishi
        ),
        afternoon = SpecialDay.getAfternoonReading(
          specialDay = specialDay,
          nextWeeklyReading = nextWeeklyReading,
          isShabbos = isShabbos
        )
      )
    }

    private def nextWeeklyReadingFor(day: Day): WeeklyReading = {
      val nextShabbos = day.shabbosAfter
      weeklyReadings.getOrElse(nextShabbos, nextWeeklyReadingFor(nextShabbos))
    }
  }

  private final case class YearData(
    year: Year,
    shabbosBereishis: Day,
    festivals: Map[Day, FestivalOrIntermediate]
  )

  // TODO we need to keep some days after 'to':
  // - next Shabbos with weekly reading for the sheni/chamishi;
  // - festival etc. for tachanun...
  private def createBuilder(from: Day, to: Day, inHolyLand: Boolean): Builder = {
    val fromYear: Year = if (ShabbosBereishis.date(from.year) <= from) from.year else from.year - 1
    val toYear: Year = if (to < ShabbosBereishis.date(to.year)) to.year else to.year + 1
    val years: Seq[Year] = Util.unfoldSimple[Year](fromYear, _ + 1, _ <= toYear)

    val yearsData: Seq[YearData] = years.map { year =>
      YearData(
        year = year,
        shabbosBereishis = ShabbosBereishis.date(year),
        festivals = SpecialDay.festivals(inHolyLand).map { specialDay => specialDay.date(year) -> specialDay }.toMap
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

    def filter[T](from: Day, to: Day, data: Seq[Map[Day, T]]): Map[Day, T] = {
      val result = data.head.filterKeys(from <= _) +: data.tail.init :+ data.last.filterKeys(_ <= to)
      result.flatten.toMap
    }

    Builder(
      from = from,
      to = to,
      inHolyLand = inHolyLand,
      weeklyReadings = filter(from, to, weeklyReadings),
      festivals = filter(from, to, yearsData.map(_.festivals)),
      years = years,
      daysWithSpecialReadingsNotFestivals = filter(from, to, years.map(year =>
        SpecialDay.daysWithSpecialReadingsNotFestivals.map(day => day.correctedDate(year) -> day).toMap)),
      specialShabboses = filter(from, to, years.map(year =>
        SpecialDay.specialShabbos.map(day => day.correctedDate(year) -> day).toMap)),
      pesachOnChamishi = years.flatMap { year =>
        if (SpecialDay.Pesach.date(year).is(Day.Name.Chamishi)) Some(year) else None
      }.toSet
    )
  }

  // Used by tests only
  def weeklyReadingsForYear(year: Year, inHolyLand: Boolean): Map[Day, WeeklyReading] =
    createBuilder(year.firstDay, year.lastDay, inHolyLand).weeklyReadings



  def printHaftarahList(custom: Custom, spec: LanguageSpec, full: Boolean): Unit = {
    println(custom.toString(spec))
    for (parsha <- Parsha.values) {
      val haftarah: Haftarah.Customs = Haftarah.forParsha(parsha)
      val customEffective: Custom = Custom.find(haftarah, custom)
      val spansOpt: Seq[ProphetSpan.BookSpan] = haftarah(customEffective)
      val result: String = ProphetSpan.toString(spansOpt, spec)

      if (customEffective == custom) {
        println(parsha.toString(spec) + ": " + result)
      } else if (full) {
        println(parsha.toString(spec) + " [" + customEffective.toString(spec)  + "]" + ": " + result)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(Aliyot.toString(Parsha.Mattos.getDaysCombined(Custom.Ashkenaz), Language.Hebrew.toSpec))
    println()
    printHaftarahList(Custom.Shami, Language.Hebrew.toSpec, full = false)
    println()
    //    println(SpecialReading.SheminiAtzeres.shabbosAliyot.get.toString(Language.English.toSpec))
    println()
    //    println(SpecialDay.SheminiAtzeres.getReading(false).maftir.get.toString(Language.English.toSpec))
    println()

    val year = Year(5779)
    val day = year.month(Month.Name.Marheshvan).day(25)
    val schedule: Schedule = apply(year, inHolyLand = false)
    val daySchedule: DaySchedule = schedule.days(day)
    println(daySchedule)
  }
}
