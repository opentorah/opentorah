package org.opentorah.schedule.tanach

import org.opentorah.calendar.Week
import org.opentorah.calendar.jewish.Jewish.{Day, Month}
import org.opentorah.calendar.jewish.Jewish.Month.*
import org.opentorah.calendar.jewish.SpecialDay
import org.opentorah.calendar.jewish.SpecialDay.*
import org.opentorah.metadata.Named
import org.opentorah.texts.tanach.{Reading, SpecialReadings, WeeklyReading}

object Readings:
  final def getMorningReading(
    day: Day,
    specialDay: Option[SpecialDay],
    specialShabbos: Option[SpecialShabbos],
    weeklyReading: Option[WeeklyReading],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] =
    val isShabbos: Boolean = day.isShabbos
    if !isShabbos then require(weeklyReading.isEmpty && specialShabbos.isEmpty)

    val result =
      if isShabbos then Some(getShabbosMorningReading(day, specialDay, weeklyReading, specialShabbos))
      else getWeekdayMorningReading(day, specialDay, nextWeeklyReading, isPesachOnChamishi)

    val numAliyot: Int =
      if specialDay.contains(SimchasTorah) then 7 else
      if specialDay.contains(SheminiAtzeresAndSimchasTorahInHolyLand) then 7 else
      if specialDay.contains(YomKippur) then 6 else
      if isShabbos then 7 else
      if specialDay.exists(_.isInstanceOf[Festival]) then 5 else
      if specialDay.exists(_.isInstanceOf[Intermediate]) then 4 else
      if day.isRoshChodesh then 4 else
      if specialDay.exists(_.isInstanceOf[RabbinicFestival]) then 3 else
      if specialDay.exists(_.isInstanceOf[Fast]) then 3 else
      if day.is(Week.Day.Sheni) || day.is(Week.Day.Chamishi) then 3 else
        0

    result.fold(require(numAliyot == 0))(_.torah.customs.values.foreach(torah => require(torah.length == numAliyot)))

    result

  final def getPurimAlternativeMorningReading(
    day: Day,
    specialDay: Option[SpecialDay],
    specialShabbos: Option[SpecialShabbos],
    weeklyReading: Option[WeeklyReading],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] =
    val isAlternative = specialDay.contains(Purim) || specialDay.contains(ShushanPurim)
    if isAlternative then getMorningReading(day, None, specialShabbos, weeklyReading, nextWeeklyReading, isPesachOnChamishi)
    else None

  private final def getShabbosMorningReading(
    day: Day,
    specialDay: Option[SpecialDay],
    weeklyReading: Option[WeeklyReading],
    specialShabbos: Option[SpecialShabbos],
  ): Reading =
    require(day.isShabbos)

    val isRoshChodesh: Boolean = day.isRoshChodesh
    val roshChodeshDay = if isRoshChodesh then Some(RoshChodesh) else None

    val weekly: Reading = specialDay.fold {
      require(weeklyReading.isDefined)
      SpecialReadings.correctKiSeitzei(
        reading = weeklyReading.get.getMorningReading,
        isMonthElul = day.month.name == Elul,
        dayNumber = day.numberInMonth
      )
    } (specialDay => getShabbosMorningReading(specialDay, weeklyReading, roshChodeshDay))

    val result: Reading = specialShabbos.fold(weekly)(specialShabbos =>
      applySpecialShabbos(specialShabbos, weekly, day, roshChodeshDay))

    val roshChodeshOf: Option[Month.Name] = day.roshChodeshOf

    val isSpecialShabbos: Boolean = specialShabbos.isDefined

    val correctedForRoshChodesh = roshChodeshOf
      .fold(result)(month => SpecialReadings.RoshChodesh.correct(
        day = RoshChodesh,
        isSpecialShabbos,
        isMonthTevesAv = (month == Teves) || (month == Av),
        isMonthElul = month == Elul,
        isMonthTishrei = month == Tishrei,
        result
      ))

    day.next.roshChodeshOf
      .fold(correctedForRoshChodesh)(month => SpecialReadings.ErevRoshChodesh.correct(
        day = ErevRoshChodesh,
        isSpecialShabbos,
        isRoshChodesh = roshChodeshOf.isDefined,
        isMonthTevesAvElul = (month == Teves) || (month == Av) || (month == Elul),
        isMonthTishrei = month == Tishrei,
        correctedForRoshChodesh
      ))

  private def getShabbosMorningReading(
    specialDay: SpecialDay,
    weeklyReading: Option[WeeklyReading],
    roshChodeshDay: Option[Named]
  ): Reading = specialDay match
    case day: Chanukah =>
      require(weeklyReading.isDefined)
      SpecialReadings.Chanukah.shabbos(
        day,
        roshChodeshDay,
        weeklyReading = weeklyReading.get,
        dayNumber = day.dayNumber
      )

    case ShushanPurim =>
      require(weeklyReading.isDefined)
      SpecialReadings.ShushanPurim.shabbos(ShushanPurim, weeklyReading.get)

    case day: SuccosIntermediate =>
      require(weeklyReading.isEmpty)
      SpecialReadings.SuccosIntermediate.shabbos(day, day.intermediateDayNumber, day.inHolyLand)

    case day =>
      val reading: SpecialReadings.ShabbosReading = day match
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
        case _ => throw IllegalArgumentException("Must have Shabbos reading!")
      require(weeklyReading.isEmpty)
      reading.shabbos(day)

  private def applySpecialShabbos(
    specialShabbos: SpecialShabbos,
    weekly: Reading,
    day: Day,
    roshChodeshDay: Option[Named]
  ): Reading = specialShabbos match
    case ShabbosHagodol =>
      SpecialReadings.ShabbosHagodol.transform(
        day = ShabbosHagodol,
        isErevPesach = day.next == Pesach1.date(day.year),
        reading = weekly
      )

    case day: SpecialParsha =>
      val reading: SpecialReadings.SpecialParsha = day match
        case ParshasShekalim  => SpecialReadings.ParshasShekalim
        case ParshasZachor    => SpecialReadings.ParshasZachor
        case ParshasParah     => SpecialReadings.ParshasParah
        case ParshasHachodesh => SpecialReadings.ParshasHachodesh
      reading.transform(weekly, day, roshChodeshDay)

  private final val sheniAndChamishi: Set[Week.Day] = Set(Week.Day.Sheni, Week.Day.Chamishi)

  private final def getWeekdayMorningReading(
    day: Day,
    specialDay: Option[SpecialDay],
    nextWeeklyReading: WeeklyReading,
    isPesachOnChamishi: Boolean
  ): Option[Reading] =
    val isRoshChodesh: Boolean = day.isRoshChodesh
    val roshChodeshDay: Option[Named] = if isRoshChodesh then Some(RoshChodesh) else None

    specialDay.map {
      case day: SuccosIntermediate =>
        SpecialReadings.SuccosIntermediate.weekday(day, day.intermediateDayNumber, day.inHolyLand)
      case day: Chanukah =>
        SpecialReadings.Chanukah.weekday(day, roshChodeshDay, day.dayNumber)
      case day: PesachIntermediate =>
        SpecialReadings.PesachIntermediate.weekday(day, isPesachOnChamishi, day.dayNumber)

      case day =>
        val reading: SpecialReadings.WeekdayReading = day match
          case RoshHashanah1 => SpecialReadings.RoshHashanah1
          case RoshHashanah2 => SpecialReadings.RoshHashanah2
          case YomKippur => SpecialReadings.YomKippur // TODO NPE?!
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
          case _ => throw IllegalArgumentException("Must have weekday reading!")
        reading.weekday(day)
    }
      .orElse(
        // TODO java.lang.NoClassDefFoundError: Could not initialize class org.opentorah.texts.tanach.SpecialReadings$RoshChodesh$
        if isRoshChodesh then Some(SpecialReadings.RoshChodesh.weekday(RoshChodesh))
        else if sheniAndChamishi.contains(day.name) then Some(nextWeeklyReading.getAfternoonReading) else None
      )

  // On Festival that falls on Shabbos, afternoon reading is that of the Shabbos - except on Yom Kippur.
  def getAfternoonReading(
    day: Day,
    specialDay: Option[SpecialDay],
    nextWeeklyReading: WeeklyReading
  ): Option[Reading] =
    val specialReading: Option[Reading] = specialDay flatMap {
      case YomKippur =>
        Some(SpecialReadings.YomKippur.afternoon(YomKippur))

      case fast: Fast =>
        val reading: SpecialReadings.Fast = fast match
          case FastOfGedalia => SpecialReadings.FastOfGedalia
          case FastOfTeves   => SpecialReadings.FastOfTeves
          case FastOfEster   => SpecialReadings.FastOfEster
          case FastOfTammuz  => SpecialReadings.FastOfTammuz
          case TishaBeAv     => SpecialReadings.TishaBeAv
        Some(reading.afternoon(fast))

      case _ => None
    }

    val result: Option[Reading] = specialReading
      .orElse(if day.isShabbos then Some(nextWeeklyReading.getAfternoonReading) else None)

    result.foreach(_.torah.customs.values.foreach(torah => require(torah.length == 3)))

    result
