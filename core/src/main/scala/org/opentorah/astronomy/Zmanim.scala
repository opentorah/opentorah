package org.opentorah.astronomy

import com.kosherjava.zmanim.ZmanimCalendar
import com.kosherjava.zmanim.util.{GeoLocation, NOAACalculator, SunTimesCalculator}
import java.time.{OffsetDateTime, ZoneOffset}
import java.util.{Calendar, Date, SimpleTimeZone}

/*
  Some calculations on the Kiddush between 6 and 7

- 7thhour.info
- Solar Data
    - https://gml.noaa.gov/grad/solcalc/
    - https://sunrise-sunset.org/api
    - https://www.hebcal.com/home/1663/zmanim-halachic-times-api
    - https://github.com/KosherJava/zmanim
    - https://kosherjava.com/zmanim-project/how-to-use-the-zmanim-api/
*/
object Zmanim:

  val calculator =
//    new SunTimesCalculator
    new NOAACalculator

  final class Location(
    name: String,
    latitude: Double,
    longitude: Double,
    rawOffset: Int
  ):
    def noSaving(date: Date): OffsetDateTime = date.toInstant.atOffset(ZoneOffset.ofHours(rawOffset))

    def minute(date: OffsetDateTime): Int = date.getHour*60 + date.getMinute

    private lazy val geoLocation: GeoLocation = GeoLocation(
      name,
      latitude,
      longitude,
      0,
      SimpleTimeZone(rawOffset, name)
    )

    def zmanim(year: Int): Seq[ZmanimCalendar] = for day <- 1 to 365 yield
      val zmanim: ZmanimCalendar = ZmanimCalendar(geoLocation)
      zmanim.setAstronomicalCalculator(calculator)
      val calendar = zmanim.getCalendar
      calendar.set(Calendar.YEAR, year)
      calendar.set(Calendar.DAY_OF_YEAR, day)
      zmanim

    def averageNoonOffset(year: Int): Int =
      val noonOffsets: Seq[Int] = for zmanim <- zmanim(year) yield
        val noon: OffsetDateTime = noSaving(zmanim.getChatzos)
        (noon.getHour-12)*60 + noon.getMinute

      (noonOffsets.sum.toFloat/noonOffsets.length).toInt

    def earlyShabbos(year: Int): Unit =
      val noonOffset = averageNoonOffset(year)
      println(s"noon offset: $noonOffset")

      for zmanim <- zmanim(year) do
        val plagHamincha = noSaving(zmanim.getPlagHamincha)
        val plagHaminchaMinute: Int = minute(plagHamincha)
        val seventhHourReal = noSaving(zmanim.getChatzos).plusHours(6)

        val minutesBeforeSeventhHourReal: Int = minute(seventhHourReal)-plagHaminchaMinute
        val minutesBeforeSeventhHourMean: Int = 18*60+noonOffset-plagHaminchaMinute

        if minutesBeforeSeventhHourReal <= 0 then
          println(s"plagHamincha: $plagHamincha; minutes: $minutesBeforeSeventhHourReal")

//        if minutesBeforeSeventhHourMean <= 0 then
//          println(s"plagHamincha: $plagHamincha; minutes: $minutesBeforeSeventhHourMean")

  val Lakewood: Location = Location("Lakewood, NJ", 40.096, -74.222, -5)
  val NewYork : Location = Location("New York, NY", 40.669, -73.943, -5) // 770
  val Boston  : Location = Location("Boston, MA"  , 42.323, -71.142, -5) // Shaloh House
  val Mohilev : Location = Location("Mohilev"     , 53.928,  30.356,  3)

  @main def run(): Unit = println(Mohilev.earlyShabbos(2000))
