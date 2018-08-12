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

//      verify(year, inHolyLand = false)
      verify(year, inHolyLand = true)
    }
  }

  def verify(year: Year, inHolyLand: Boolean): Unit = {
    val readings: Seq[(Day, Readings)] = Readings.readings(year, inHolyLand)

    def findReadings(day: Day): Readings = readings.find(_._1 == day).get._2
    def isCombined(parsha: Parsha): Boolean = readings.exists(_._2.secondParsha.contains(parsha))

    val readingsBeforePesach: Readings = findReadings(SpecialDay.shabbosBefore(SpecialDay.Pesach(year)))
    readingsBeforePesach.isCombined shouldBe false
    // TODO in the Holy Land sometimes Vayikra is read on Shabbos before Pesach;
    // either my algorithm is wrong - or Shulchan Oruch is only talking about Diaspora...
    readingsBeforePesach.parsha shouldBe (if (!year.isLeap) Tzav else {
      val roshHaShonohOnChamishi: Boolean = SpecialDay.RoshHashanah(year).name == Day.Name.Chamishi
      val bothCheshvanAndKislevFullOrLacking = year.kind != Year.Kind.Regular
      if (roshHaShonohOnChamishi && bothCheshvanAndKislevFullOrLacking) AchareiMot else Metzora
    })

    val readingsBeforeShavuot = findReadings(SpecialDay.shabbosBefore(SpecialDay.Shavuot(year)))
    readingsBeforeShavuot.isCombined shouldBe false
    val parshaBeforeShavuot = readingsBeforeShavuot.parsha
    (parshaBeforeShavuot == Bemidbar || parshaBeforeShavuot == Naso) shouldBe true

    findReadings(SpecialDay.shabbosAfter(SpecialDay.TishaBav(year))) shouldBe Readings(Vaetchanan)

    findReadings(SpecialDay.shabbosBefore(SpecialDay.RoshHashanah(year+1))).parsha shouldBe Nitzavim
    val roshHaShanahDay: Day.Name = SpecialDay.RoshHashanah(year+1).name
    isCombined(Vayelech) shouldBe (roshHaShanahDay != Day.Name.Sheni) && (roshHaShanahDay != Day.Name.Shlishi)
  }
}
