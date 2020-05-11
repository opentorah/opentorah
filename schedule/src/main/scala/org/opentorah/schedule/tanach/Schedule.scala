package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish.{Day, Year}
import org.opentorah.metadata.WithNames
import org.opentorah.texts.tanach.{Reading, WeeklyReading}
import org.opentorah.util.{Cache, Collections, PairSlider}
import SpecialDay.{FestivalOrIntermediate, Omer, ShabbosBereishis}

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
    purimAlternativeMorning: Option[Reading],
    afternoon: Option[Reading],
    chitas: Chitas
  )

  def get(day: Day, inHolyLand: Boolean): DaySchedule = {
    val schedule = Schedule(from = day, to = day, inHolyLand)
    schedule.days(day)
  }

  def apply(from: Day, to: Day, inHolyLand: Boolean): Schedule = createBuilder(from, to, inHolyLand).build

  def apply(year: Year, inHolyLand: Boolean): Schedule = Schedule(year.firstDay, year.lastDay, inHolyLand)

  private final case class Builder private(
    from: Day,
    to: Day,
    inHolyLand: Boolean,
    weeklyReadings: Map[Day, WeeklyReading],
    weeklyReadingsList: Seq[(Day, WeeklyReading)],
    festivals: Map[Day, FestivalOrIntermediate],
    daysWithSpecialReadingsNotFestivals: Map[Day, SpecialDay.Date],
    specialShabboses: Map[Day, SpecialDay.SpecialShabbos]
  ) {
    def build: Schedule = {
      currentWeeklyReadings.reset()
      nextWeeklyReadings.reset()

      val days: Seq[Day] = Collections.unfoldSimple[Day](from, _ + 1, _ <= to)
      new Schedule(from, to, inHolyLand, days = days.map(day => day -> forDay(day, inHolyLand)).toMap)
    }

    private val currentWeeklyReadings = new PairSlider[Day, WeeklyReading](weeklyReadingsList, _ >= _)

    private val nextWeeklyReadings = new PairSlider[Day, WeeklyReading](weeklyReadingsList, _ > _)

    private val pesachOnChamishi = new Cache[Year, Boolean] {
      override def calculate(year: Year): Boolean = SpecialDay.Pesach1.date(year).is(Day.Name.Chamishi)
    }

    private def forDay(day: Day, inHolyLand: Boolean): DaySchedule = {
      val weeklyReading: Option[WeeklyReading] = weeklyReadings.get(day)
      val specialDay: Option[SpecialDay.Date] = festivals.get(day).orElse(daysWithSpecialReadingsNotFestivals.get(day))
      val specialShabbos: Option[SpecialDay.SpecialShabbos] = specialShabboses.get(day)
      val nextWeeklyReading: WeeklyReading = nextWeeklyReadings.get(day)

      DaySchedule(
        day,
        dayNames =
          specialDay.toSeq ++
          specialShabbos.toSeq ++
          (if (day.next.isRoshChodesh) Seq(SpecialDay.ErevRoshChodesh) else Seq.empty) ++
          (if (day.isRoshChodesh) Seq(SpecialDay.RoshChodesh) else Seq.empty) ++
          Omer.dayOf(day).toSeq,
        morning = Readings.getMorningReading(
          day = day,
          specialDay = specialDay,
          specialShabbos = specialShabbos,
          weeklyReading = weeklyReading,
          nextWeeklyReading = nextWeeklyReading,
          isPesachOnChamishi = pesachOnChamishi.get(day.year)
        ),
        purimAlternativeMorning = Readings.getPurimAlternativeMorningReading(
          day = day,
          specialDay = specialDay,
          specialShabbos = specialShabbos,
          weeklyReading = weeklyReading,
          nextWeeklyReading = nextWeeklyReading,
          isPesachOnChamishi = pesachOnChamishi.get(day.year)
        ),
        afternoon = Readings.getAfternoonReading(
          day = day,
          specialDay = specialDay,
          nextWeeklyReading = nextWeeklyReading
        ),
        chitas = Chitas(day, currentWeeklyReadings.get(day), inHolyLand)
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
    val toYear: Year = to.year + 1
    val years: Seq[Year] = Collections.unfoldSimple[Year](fromYear, _ + 1, _ <= toYear)

    val yearsData: Seq[YearData] = years.map { year => YearData(
      year = year,
      shabbosBereishis = ShabbosBereishis.date(year),
      festivals = SpecialDay.festivals(inHolyLand).map { specialDay => specialDay.date(year) -> specialDay }
    )}

    val weeklyReadingCycles: Seq[Seq[(Day, WeeklyReading)]] = (yearsData zip yearsData.tail).map { case (current, next) =>
      WeeklyReadingCalculator.getCycle(
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

    def filterFirst[T](data: Set[(Day, T)]): Set[(Day, T)] = data.filter(from <= _._1)
    def filterLast[T](data: Set[(Day, T)], to: Day): Set[(Day, T)] = data.filter(_._1 <= to)
    def filter[T](data: Seq[Set[(Day, T)]], to: Day): Map[Day, T] =
      (filterFirst(data.head) +: data.tail.init :+ filterLast(data.last, to)).flatten.toMap
    def filterDates[T <: SpecialDay.Date](dates: Set[T]): Map[Day, T] =
      filter(years.map(year => dates.map(day => day.correctedDate(year) -> day)), to)

    Builder(
      from = from,
      to = to,
      inHolyLand = inHolyLand,
      weeklyReadings = weeklyReadings.toMap,
      weeklyReadingsList = weeklyReadings :+ extraWeeklyReading,
      festivals = filter(yearsData.map(_.festivals), to+7), // to get festivals for Tachanun and Motzoei Shabbos
      daysWithSpecialReadingsNotFestivals = filterDates(Readings.daysWithSpecialReadingsNotFestivals),
      specialShabboses = filterDates(SpecialDay.specialShabbos)
    )
  }

  // Used by tests only
  def weeklyReadingsForYear(year: Year, inHolyLand: Boolean): Map[Day, WeeklyReading] =
    createBuilder(year.firstDay, year.lastDay, inHolyLand).weeklyReadings
}
