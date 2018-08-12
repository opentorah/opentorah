package org.podval.calendar.generate.chumash

import org.scalatest.{FlatSpec, Matchers}
import org.podval.calendar.jewish.{Jewish, SpecialDay}
import Jewish.{Day, Year}
import Parsha._

// TODO check that fixed parshios are read on the days they should
// TODO check simanim from the Shulchan Oruch
class ReadingsTest extends FlatSpec with Matchers {

  // Shulchan Aruch, Orach Chaim, 428:3; Rambam TODO
  "Torah readings" should "be assigned correctly" in {
    // Some properties are built into the algorithm in an obvious way:
    // - no regular readings on festivals and intermediate days;
    // - cycle runs from one Shabbos Breshit to the next;
    // - on Shabbos before Shavuot read Bemidbar;
    // - on Shabbos before or on Tisha Be Av read Devarim;
    // - priorities of combining the portions.
    (1 to 6000) foreach { number =>
      val year = Year(number)
      println(year)

      val readings = Readings.readings(year, inHolyLand = false)
      val readingsInHolyLand = Readings.readings(year, inHolyLand = true)

      verifyShabbosBeforePesach(year, readings)

      // TODO in the Holy Land sometimes Vayikra is read on Shabbos before Pesach;
      // either my algorithm is wrong - or Shulchan Oruch is only talking about Diaspora...
      // verifyShabbosBeforePesach(year, readingsInHolyLand)

      // verifyVayelech(year, readings)
    }
  }

  def verifyShabbosBeforePesach(year: Year, readings: Seq[(Day, Readings)]): Unit = {
    val shabbosBeforePesach = SpecialDay.Pesach(year).prev.prev(Day.Name.Shabbos)
    val readingsBeforePesach: Readings = findReadings(readings, shabbosBeforePesach)
    readingsBeforePesach.isCombined shouldBe false
    val correctParsha: Parsha = if (!year.isLeap) Tzav else {
      val roshHaShonohOnChamishi: Boolean = SpecialDay.RoshHashanah(year).name == Day.Name.Chamishi
      val bothCheshvanAndKislevFullOrLacking = year.kind != Year.Kind.Regular
      if (roshHaShonohOnChamishi && bothCheshvanAndKislevFullOrLacking) AchareiMot else Metzora
    }
    readingsBeforePesach.parsha shouldBe correctParsha
  }

  def verifyVayelech(year: Year, readings: Seq[(Day, Readings)]): Unit = {
    val roshHaShonohDay: Day.Name = SpecialDay.RoshHashanah(year+1).name
    val roshHaShonohOnSheniOrShlishi: Boolean =
      (roshHaShonohDay == Day.Name.Sheni) || (roshHaShonohDay == Day.Name.Shlishi)

    isCombined(readings, Vayelech) shouldBe roshHaShonohOnSheniOrShlishi
  }

  def findReadings(readings: Seq[(Day, Readings)], day: Day): Readings =
    readings.find(_._1 == day).get._2

  def isCombined(readings: Seq[(Day, Readings)], parsha: Parsha): Boolean =
    readings.exists(_._2.secondParsha.contains(parsha))
}
