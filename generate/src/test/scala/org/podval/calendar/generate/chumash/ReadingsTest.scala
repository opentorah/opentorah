package org.podval.calendar.generate.chumash

import org.scalatest.{FlatSpec, Matchers}
import org.podval.calendar.jewish.{Jewish, SpecialDay}
import Jewish.Day

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
    //
    // So seems combinig Nitzavim/Vayelech, which can make a nice, albeit redundant, test: TODO
    //  def beforeRoshHaShonoh(year: Year): Readings = {
    //    val roshHaShonohDay: Day.Name = SpecialDay.RoshHashanah(year).name
    //    val roshHaShonohOnSheniOrShlishi: Boolean =
    //      (roshHaShonohDay == Day.Name.Sheni) || (roshHaShonohDay == Day.Name.Shlishi)
    //    Readings(
    //      parsha = Nitzavim,
    //      secondParsha = if (roshHaShonohOnSheniOrShlishi) None else Some(Vayelech)
    //    )
    //  }
    //
    // TODO
    // But this thing about Pesach - is it an emergent phenomenon - or an additional pinning requirement?
    //  def beforePassover(year: Year): Parsha = if (!year.isLeap) Tzav else {
    //    val roshHaShonohOnChamishi: Boolean = SpecialDay.RoshHashanah(year).name == Day.Name.Chamishi
    //    val bothCheshvanAndKislevFullOrLacking = year.kind != Year.Kind.Regular
    //    if (roshHaShonohOnChamishi && bothCheshvanAndKislevFullOrLacking) AchareiMot else Metzora
    //  }

    def findReadings(readings: Seq[(Day, Readings)], day: Day): Readings = readings.find(_._1 == day).get._2

    // TODO too many weeks after Devarim for year 1; 34 for year 3
    (5775 to 5789) foreach { number =>
      val year = Jewish.Year(number)
      val shabbosBeforePesach = SpecialDay.Pesach(year).prev.prev(Day.Name.Shabbos)

      val readings = Readings.readings(year, inHolyLand = false)
//      val readingsInHolyLand = Readings.readings(year, inHolyLand = true)


      println(year + " " + findReadings(readings, shabbosBeforePesach))
//      println(findReadings(readingsInHolyLand, shabbosBeforePesach))
    }
  }
}
