package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish.{Day, Month}
import org.opentorah.calendar.jewish.Jewish.Month.Name._
import org.opentorah.calendar.jewish.SpecialDay._
import org.opentorah.texts.tanach.{Custom, Haftarah, Parsha, Reading, SpecialReadings, WeeklyReading}

object Readings {
  val daysWithSpecialReadingsNotFestivals: Set[Date] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[Date] = festivals(inHolyLand) ++ daysWithSpecialReadingsNotFestivals

  final def getMorningReading(
    day: Day,
    specialDay: Option[Date],
    specialShabbos: Option[SpecialShabbos],
    weeklyReading: Option[WeeklyReading],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] = {
    val isShabbos: Boolean = day.isShabbos
    if (!isShabbos) require(weeklyReading.isEmpty && specialShabbos.isEmpty)

    val result =
      if (isShabbos) Some(getShabbosMorningReading(day, specialDay, weeklyReading, specialShabbos))
      else getWeekdayMorningReading(day, specialDay, nextWeeklyReading, isPesachOnChamishi)

    val numAliyot: Int =
      if (specialDay.contains(SimchasTorah)) 7 else
      if (specialDay.contains(SheminiAtzeresAndSimchasTorahInHolyLand)) 7 else
      if (specialDay.contains(YomKippur)) 6 else
      if (isShabbos) 7 else
      if (specialDay.exists(_.isInstanceOf[Festival])) 5 else
      if (specialDay.exists(_.isInstanceOf[Intermediate])) 4 else
      if (day.isRoshChodesh) 4 else
      if (specialDay.exists(_.isInstanceOf[RabbinicFestival])) 3 else
      if (specialDay.exists(_.isInstanceOf[Fast])) 3 else
      if (day.is(Day.Name.Sheni) || day.is(Day.Name.Chamishi)) 3 else
        0

    result.fold(require(numAliyot == 0)) { _.torah.customs.values.foreach { torah => require(torah.length == numAliyot) }}

    result
  }

  final def getPurimAlternativeMorningReading(
    day: Day,
    specialDay: Option[Date],
    specialShabbos: Option[SpecialShabbos],
    weeklyReading: Option[WeeklyReading],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] = {
    val isAlternative = specialDay.contains(Purim) || specialDay.contains(ShushanPurim)
    if (!isAlternative) None else getMorningReading(day, None, specialShabbos, weeklyReading, nextWeeklyReading, isPesachOnChamishi)
  }

  private final def getShabbosMorningReading(
    day: Day,
    specialDay: Option[Date],
    weeklyReading: Option[WeeklyReading],
    specialShabbos: Option[SpecialShabbos],
  ): Reading = {
    require(day.isShabbos)

    val isRoshChodesh: Boolean = day.isRoshChodesh

    val normal: Reading = specialDay.map {
      case day: Chanukah =>
        require(weeklyReading.isDefined)
        SpecialReadings.Chanukah.shabbos(
          day,
          roshChodeshDay = if (isRoshChodesh) Some(RoshChodesh) else None,
          weeklyReading = weeklyReading.get,
          dayNumber = day.dayNumber
        )

      case ShushanPurim =>
        require(weeklyReading.isDefined)
        SpecialReadings.ShushanPurim.shabbos(ShushanPurim, weeklyReading.get)

      // TODO for all below:  require(weeklyReading.isEmpty)

      case day: SuccosIntermediate =>
        SpecialReadings.SuccosIntermediate.shabbos(day, day.intermediateDayNumber, day.inHolyLand)

      case day =>
        val reading: SpecialReadings.ShabbosReading = day match {
          case _: PesachIntermediate => SpecialReadings.PesachIntermediate
          case RoshHashanah1 => SpecialReadings.RoshHashanah1
          case YomKippur => SpecialReadings.YomKippur
          case Succos1 => SpecialReadings.Succos1
          case Succos2 => SpecialReadings.Succos2
          case SheminiAtzeres => SpecialReadings.SheminiAtzeres
          case SheminiAtzeresAndSimchasTorahInHolyLand => SpecialReadings.SheminiAtzeresAndSimchasTorahInHolyLand
          case Pesach1 => SpecialReadings.Pesach1
          case Pesach7 => SpecialReadings.Pesach7
          case Pesach8 => SpecialReadings.Pesach8
          case Shavuos2 => SpecialReadings.Shavuos2
          case _ => throw new IllegalArgumentException("Must have Shabbos reading!")
        }
        reading.shabbos(day)
    }
      .getOrElse {
        require(weeklyReading.isDefined)
        val result = weeklyReading.get.getMorningReading
        val isKiSeitzei: Boolean = (day.month.name == Elul) && (day.numberInMonth == 14)
        if (!isKiSeitzei) result else {
          val customs: Custom.Of[Reading.ReadingCustom] = result.liftR {
            case (custom: Custom, readingCustom: Reading.ReadingCustom) =>
              if (custom != Custom.Chabad) readingCustom
              else readingCustom.addHaftarah(Haftarah.forParsha(Parsha.Re_eh).doFind(Custom.Chabad))
          }
          new Reading(customs.customs)
        }
      }

    val result = specialShabbos.fold(normal) {
      case ShabbosHagodol =>
        SpecialReadings.ShabbosHagodol.transform(
          day = ShabbosHagodol,
          isErevPesach = day.next == Pesach1.date(day.year),
          reading = normal
        )

      case specialParsha: SpecialParsha =>
        val reading = specialParsha match {
          case ParshasShekalim  => SpecialReadings.ParshasShekalim
          case ParshasZachor    => SpecialReadings.ParshasZachor
          case ParshasParah     => SpecialReadings.ParshasParah
          case ParshasHachodesh => SpecialReadings.ParshasHachodesh
        }
        reading.transform(normal, specialParsha, if (isRoshChodesh) Some(RoshChodesh) else None) // TODO name of the month?
    }

    val roshChodeshOf: Option[Month.Name] = {
      val result = day.roshChodeshOf
      // We do not mention Rosh Chodesh on Rosh Hashanah
      if (result.contains(Tishrei)) None else result
    }

    val isSpecialShabbos: Boolean = specialShabbos.isDefined

    val correctedForRoshChodesh = roshChodeshOf
      .fold(result) { month => SpecialReadings.RoshChodesh.correct(
        day = RoshChodesh,
        isSpecialShabbos,
        isMonthTevesAv = (month == Teves) || (month == Av),
        isMonthElul = month == Elul,
        result
      )}

    day.next.roshChodeshOf
      .fold(correctedForRoshChodesh) { month => SpecialReadings.ErevRoshChodesh.correct(
        day = ErevRoshChodesh,
        isSpecialShabbos,
        isRoshChodesh = roshChodeshOf.isDefined,
        isMonthTevesAvElul = (month == Teves) || (month == Av) || (month == Elul),
        correctedForRoshChodesh
      )}
  }

  private final val sheniAndChamishi: Set[Day.Name] = Set(Day.Name.Sheni, Day.Name.Chamishi)

  private final def getWeekdayMorningReading(
    day: Day,
    specialDay: Option[Date],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] = {
    val isRoshChodesh: Boolean = day.isRoshChodesh

    val specialReading: Option[Reading] = specialDay.map {
      case day: SuccosIntermediate => SpecialReadings.SuccosIntermediate.weekday(day, day.intermediateDayNumber, day.inHolyLand)
      case day: Chanukah => SpecialReadings.Chanukah.weekday(day, if (isRoshChodesh) Some(RoshChodesh) else None, day.dayNumber)
      case day: PesachIntermediate => SpecialReadings.PesachIntermediate.weekday(day, isPesachOnChamishi, day.dayNumber)

      case day =>
        val reading: SpecialReadings.WeekdayReading = day match {
          case RoshHashanah1 => SpecialReadings.RoshHashanah1
          case RoshHashanah2 => SpecialReadings.RoshHashanah2
          case YomKippur => SpecialReadings.YomKippur
          case Succos1 => SpecialReadings.Succos1
          case Succos2 => SpecialReadings.Succos2
          case SheminiAtzeres => SpecialReadings.SheminiAtzeres
          case SimchasTorah => SpecialReadings.SimchasTorah
          case SheminiAtzeresAndSimchasTorahInHolyLand => SpecialReadings.SheminiAtzeresAndSimchasTorahInHolyLand
          case Purim => SpecialReadings.Purim
          case ShushanPurim => SpecialReadings.ShushanPurim
          case Pesach1 => SpecialReadings.Pesach1
          case Pesach2 => SpecialReadings.Pesach2
          case Pesach7 => SpecialReadings.Pesach7
          case Pesach8 => SpecialReadings.Pesach8
          case Shavuos1 => SpecialReadings.Shavuos1
          case Shavuos2 => SpecialReadings.Shavuos2
          case _ => throw new IllegalArgumentException("Must have weekday reading!")
        }
        reading.weekday(day)
    }

    specialReading
      .orElse { if (!isRoshChodesh) None else Some(SpecialReadings.RoshChodesh.weekday(RoshChodesh)) }
      .orElse { if (!sheniAndChamishi.contains(day.name)) None else Some(nextWeeklyReading.getAfternoonReading) }
  }

  // On Festival that falls on Shabbos, afternoon reading is that of the Shabbos - except on Yom Kippur.
  def getAfternoonReading(
    day: Day,
    specialDay: Option[Date],
    nextWeeklyReading: WeeklyReading
  ): Option[Reading] = {
    val specialReading: Option[Reading] = specialDay flatMap {
      case YomKippur => Some(SpecialReadings.YomKippur.afternoon(YomKippur))

      case fast: Fast =>
        val reading: SpecialReadings.Fast = fast match {
          case FastOfGedalia => SpecialReadings.FastOfGedalia
          case FastOfTeves   => SpecialReadings.FastOfTeves
          case FastOfEster   => SpecialReadings.FastOfEster
          case FastOfTammuz  => SpecialReadings.FastOfTammuz
          case TishaBeAv     => SpecialReadings.TishaBeAv
        }
        Some(reading.afternoon(fast))

      case _ => None
    }

    val result = specialReading.orElse { if (!day.isShabbos) None else Some(nextWeeklyReading.getAfternoonReading) }

    result.foreach { _.torah.customs.values.foreach { torah => require(torah.length == 3) }}

    result
  }
}
