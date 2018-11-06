package org.podval.calendar.schedule.tanach

import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.calendar.schedule.tanach.SpecialDay.{FestivalOrIntermediate, RoshChodesh, ShabbosBereishis}
import org.podval.judaica.metadata.{Language, LanguageSpec}
import org.podval.judaica.metadata.tanach.{Aliyot, Custom, Parsha}
import org.podval.judaica.metadata.tanach.BookSpan.ProphetSpan
import org.podval.judaica.metadata.Util

// TODO add special parshiot, Shabbos hagodol...

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

  /*
    // TODO add special parshios, Shabbos Hagodol, erev rosh chodesh...
     Shabbos  Rosh Chodesh Additional Haftorah

     on Shabos rosh hodesh Menachem Ov  - only maftir rosh chodesh, haftarah - current and the added piece;
     on Shabbos if Sunday is also Rosh Chodesh: add "Shabbos Erev Rosh Chodesh Additional Haftorah".

     If Rosh Chodesh Elul is Shabbos, on 14th something merges...

   TODO RULE: When another haftarah (e.g., Shekalim) takes over, add "Shabbos [Erev] Rosh Chodesh Additional Haftorah"
             Chabad: always
             Fes: only EREV rosh chodesh
             остальные не добавляют

Какой-то шабат выпал на канун рош-ходеша.
Широкий обычай - читают афтару кануна рош ходеша.
Обычай Феса - читают афтару недельной главы и добавляют эту добавку - первый и последний стих из афтары кануна рош-ходеша.
Для широкого обычая есть ситуации, когда афтара недельной главы отодвигаются по другой причине.
Это не только канун рош ходеша, но и рош ходеш или суббота перед рош ходеш адар (Шкалим) или суббота перед рош-ходеш нисан (Аходеш).
В этом случае  в Хабаде читают афтару этого более сильного случая (рош ходеша, Шкалим или Аходеш) и добавляют эту добавку.
Особый случай рош ходеш менахем ав или его канун и канун рош ходеш элул.
В этом случае читают афтару недельной главы (которая на самом деле из трех увещеваний или семи утешений),
а в конце добавляют эти два стиха из афтары кануна рош ходеша.

  */
  private final def getReading(
    day: Day,
    weeklyReading: Option[WeeklyReading],
    specialDay: Option[SpecialDay.WithReading],
    isPesachOnChamishi: Boolean
  ): DaySchedule = {
    val isShabbos: Boolean = day.isShabbos
    val isRoshChodesh: Boolean = day.isRoshChodesh

    // TODO we need the next weekly reading to determine the sheni/chamishi one
    // TODO On Festival that falls on Shabbos, afternoon reading is that of the Shabbos - except on Yom Kippur.
    val afternoonSpecial: Option[Reading] = specialDay flatMap { specialDay => specialDay.getAfternoonReading }

    val specialReading: Option[Reading] = specialDay.map { specialDay =>
      val result = specialDay.getReading(isShabbos, weeklyReading, isPesachOnChamishi)
      val normalAliyot = result.aliyot


      val aliyot = if (isRoshChodesh) {
        // Applies only to Channukah - or the same happems on Shekalim. Hachodesh?
        if (!isShabbos) RoshChodesh.in3aliyot :+ normalAliyot.last else {
          // TODO for all customs:
          //   normalAliyot.take(5) :+ (normalAliyot(5)+normalAliyot(6))
          //   maftir = Some(RoshChodesh.shabbosMaftir),
          //   add Shabbos Rosh Chodesh Haftara to the one there already
        }
      } else normalAliyot

      result// TODO.copy(aliyot = aliyot)
    }

    val morningReading: Option[Reading] = specialReading.orElse(weeklyReading.map(_.getReading))

    DaySchedule(
      day,
      morning = morningReading,
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
      val haftarah: Haftarah.OptionalCustoms = Haftarah.forParsha(parsha)
      val customEffective: Custom = Custom.find(haftarah, custom)
      val spansOpt: Option[Seq[ProphetSpan.BookSpan]] = haftarah(customEffective)
      val result: String = spansOpt.fold("")(spans => ProphetSpan.toString(spans, spec))

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
    println(SpecialDay.SheminiAtzeres.getReading(false).maftir.get.toString(Language.English.toSpec))
    println()

    val x = SpecialDay.RoshHashanah1.aliyot(true)

    val year = Year(5779)
    val day = year.month(Month.Name.Marheshvan).day(25)
    val schedule: Schedule = forYear(year, inHolyLand = false)
    val daySchedule = schedule.days(day)
    println(daySchedule)
  }
}
