package org.podval.calendar.schedule.tanach

import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.schedule.tanach.SpecialDay.{FestivalOrIntermediate, ShabbosBereishis, Omer}
import org.podval.judaica.metadata.{Util, WithNames}

final case class Schedule private(
  from: Day,
  to: Day,
  inHolyLand: Boolean,
  days: Map[Day, Schedule.DaySchedule]
)

object Schedule {

  final case class DaySchedule private(
    day: Day,
    dayNames: Seq[WithNames],
    morning: Option[Reading],
    afternoon: Option[Reading],
    chitas: Chitas
  )

  def apply(from: Day, to: Day, inHolyLand: Boolean): Schedule = createBuilder(from, to, inHolyLand).build

  def apply(year: Year, inHolyLand: Boolean): Schedule = Schedule(year.firstDay, year.lastDay, inHolyLand)

  private final case class Builder private(
    from: Day,
    to: Day,
    inHolyLand: Boolean,
    weeklyReadings: Map[Day, WeeklyReading],
    weeklyReadingsList: Seq[(Day, WeeklyReading)],
    festivals: Map[Day, FestivalOrIntermediate],
    years: Seq[Year],
    daysWithSpecialReadingsNotFestivals: Map[Day, SpecialDay.Date],
    specialShabboses: Map[Day, SpecialDay.SpecialShabbos],
    pesachOnChamishi: Set[Year] // TODO calculate on the spot via a advancing memoized holder :)
  ) {
    def build: Schedule = {
      currentWeeklyReadings = weeklyReadingsList
      nextWeeklyReadings = weeklyReadingsList

      val days: Seq[Day] = Util.unfoldSimple[Day](from, _ + 1, _ <= to)
      new Schedule(from, to, inHolyLand, days = days.map(day => day -> forDay(day)).toMap)
    }

    private var currentWeeklyReadings: Seq[(Day, WeeklyReading)] = weeklyReadingsList
    private var nextWeeklyReadings: Seq[(Day, WeeklyReading)] = weeklyReadingsList

    private def currentWeeklyReadings(day: Day): WeeklyReading = {
      currentWeeklyReadings = currentWeeklyReadings.dropWhile(_._1 < day)
      currentWeeklyReadings.head._2
    }

    private def nextWeeklyReadings(day: Day): WeeklyReading = {
      nextWeeklyReadings = nextWeeklyReadings.dropWhile(_._1 <= day)
      nextWeeklyReadings.head._2
    }

    private def forDay(day: Day): DaySchedule = {
      val weeklyReading: Option[WeeklyReading] = weeklyReadings.get(day)
      val specialDay: Option[SpecialDay.Date] = festivals.get(day).orElse(daysWithSpecialReadingsNotFestivals.get(day))
      val specialShabbos: Option[SpecialDay.SpecialShabbos] = specialShabboses.get(day)
      val nextWeeklyReading: WeeklyReading = nextWeeklyReadings(day)

      DaySchedule(
        day,
        dayNames =
          specialDay.toSeq ++
          specialShabbos.toSeq ++
          (if (day.next.isRoshChodesh) Seq(SpecialDay.ErevRoshChodesh) else Seq.empty) ++
          (if (day.isRoshChodesh) Seq(SpecialDay.RoshChodesh) else Seq.empty) ++
          Omer.dayOf(day).toSeq,
        morning = SpecialDay.getMorningReading(
          day = day,
          specialDay = specialDay,
          specialShabbos = specialShabbos,
          weeklyReading = weeklyReading,
          nextWeeklyReading = nextWeeklyReading,
          isPesachOnChamishi = pesachOnChamishi.contains(day.year)
        ),
        afternoon = SpecialDay.getAfternoonReading(
          day = day,
          specialDay = specialDay,
          nextWeeklyReading = nextWeeklyReading
        ),
        chitas = Chitas(day, currentWeeklyReadings(day))
      )
    }
  }

  private final case class YearData(
    year: Year,
    shabbosBereishis: Day,
    festivals: Set[(Day, FestivalOrIntermediate)]
  ) {
    def festivalsSet: Set[Day] = festivals.map(_._1)
  }

  private def createBuilder(from: Day, to: Day, inHolyLand: Boolean): Builder = {
    val fromYear: Year = if (ShabbosBereishis.date(from.year) <= from) from.year else from.year - 1
    val toYear: Year = /* we need WeeklyReading after 'to' if (to < ShabbosBereishis.date(to.year)) to.year else */
      to.year + 1
    val years: Seq[Year] = Util.unfoldSimple[Year](fromYear, _ + 1, _ <= toYear)

    val yearsData: Seq[YearData] = years.map { year => YearData(
      year = year,
      shabbosBereishis = ShabbosBereishis.date(year),
      festivals = SpecialDay.festivals(inHolyLand).map { specialDay => specialDay.date(year) -> specialDay }
    )}

    val weeklyReadingCycles: Seq[Seq[(Day, WeeklyReading)]] = (yearsData zip yearsData.tail).map { case (current, next) =>
      WeeklyReading.getCycle(
        year = current.year,
        fromShabbosBereishis = current.shabbosBereishis,
        toShabbosBereishis = next.shabbosBereishis,
        festivals = current.festivalsSet ++ next.festivalsSet
      )
    }

    // We need next weekly reading after 'to' for sheni/chamishi/Shabbos afternoon.
    val (weeklyReadings: Seq[(Day, WeeklyReading)], extraWeeklyReading: (Day, WeeklyReading)) = {
      def span(what: Seq[(Day, WeeklyReading)]): (Seq[(Day, WeeklyReading)], (Day, WeeklyReading)) = {
        val (init, rest) = what.span(_._1 <= to)
        (init, rest.head)
      }

      val first: Seq[(Day, WeeklyReading)] = weeklyReadingCycles.head.filter(from <= _._1)
      if (weeklyReadingCycles.length == 1) span(first) else {
        val init = if (weeklyReadingCycles.length == 2) first else first ++ weeklyReadingCycles.tail.init.flatten
        val (last, extra) = span(weeklyReadingCycles.last)
        (init ++ last, extra)
      }
    }

    // TODO we also need festivals after 'to' for tachanun, maariv motzoei Shabbos...

    def filter[T](data: Seq[Set[(Day, T)]]): Map[Day, T] = {
      val result = data.head.filter(from <= _._1) +: data.tail.init :+ data.last.filter(_._1 <= to)
      result.flatten.toMap
    }

    Builder(
      from = from,
      to = to,
      inHolyLand = inHolyLand,
      weeklyReadings = weeklyReadings.toMap,
      weeklyReadingsList = weeklyReadings :+ extraWeeklyReading,
      festivals = filter(yearsData.map(_.festivals)),
      years = years,
      daysWithSpecialReadingsNotFestivals = filter(years.map(year =>
        SpecialDay.daysWithSpecialReadingsNotFestivals.map(day => day.correctedDate(year) -> day))),
      specialShabboses = filter(years.map(year =>
        SpecialDay.specialShabbos.map(day => day.correctedDate(year) -> day))),
      pesachOnChamishi = years.flatMap { year =>
        if (SpecialDay.Pesach.date(year).is(Day.Name.Chamishi)) Some(year) else None
      }.toSet
    )
  }

  // Used by tests only
  def weeklyReadingsForYear(year: Year, inHolyLand: Boolean): Map[Day, WeeklyReading] =
    createBuilder(year.firstDay, year.lastDay, inHolyLand).weeklyReadings
}
