package org.podval.calendar.generate.chumash

import org.scalatest.{FlatSpec, Matchers}
import org.podval.calendar.jewish.{Jewish, SpecialDay}
import Jewish.Day

// TODO check that fixed parshios are read on the days they should
// TODO check simanim from the Shulchan Oruch
class ReadingsTest extends FlatSpec with Matchers {

  // Shulchan Aruch, Orach Chaim, 428:3; Rambam TODO
  "Torah readings" should "be assigned correctly" in {
    def findReadings(readings: Seq[(Day, Readings)], day: Day): Readings = readings.find(_._1 == day).get._2

    // TODO too many weeks after Devarim for year 1; 34 for year 3
    (5778 to 5778) foreach { number =>
      val year = Jewish.Year(number)
      println(year)
      val readings = Readings.readings(year, inHolyLand = false)
      val readingsInHolyLand = Readings.readings(year, inHolyLand = true)

      // Some properties are built into the algorithm in an obvious way:
      // - no regular readings on festivals and intermediate days;
      // - cycle runs from one Shabbos Breshit to the next;
      // - on Shabbos before Shavuot read Bemidbar;
      // - on Shabbos before or on Tisha Be Av read Devarim;
      // - priorities of combining the portions.
      val shabbosBeforePesach = SpecialDay.Pesach(year).prev.prev(Day.Name.Shabbos)

      println(findReadings(readings, shabbosBeforePesach))
      println(findReadings(readingsInHolyLand, shabbosBeforePesach))
    }
  }
}
