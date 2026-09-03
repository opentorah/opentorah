package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish.{Day, Month, Year}
import org.opentorah.calendar.jewish.Jewish.Month.*
import org.opentorah.texts.tanach.{Custom, Torah}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * When Rosh Chodesh falls on Shabbos, Numbers 28:9-15 is read as the maftir --
 * and that does not depend on which haftarah is read after it.
 *
 * In Av the rebuke is read and in Elul the consolation, each in sequence with
 * the weeks around it, and neither gives way to the Rosh Chodesh haftarah
 * (Chabad is the exception in Elul). The maftir is unaffected: it is Rosh
 * Chodesh's in those months as in any other. Only where something else has
 * already taken the maftir -- one of the four parshiyos, or Chanukah, inside
 * which Rosh Chodesh Teves always falls -- is it not read as the maftir, and
 * there it is read as the seventh aliyah instead.
 */
final class RoshChodeshMaftirTest extends AnyFlatSpec, Matchers:

  private val years: Range = 5700 to 5900

  /** Numbers 28:9-15, stated here rather than read from the data the code
    * under test reads, so that the two have to agree. */
  private val expected: (String, Int, Int, Int, Int) = ("Numbers", 28, 9, 28, 15)

  private def maftirOn(day: Day, inHolyLand: Boolean, custom: Custom): Option[Torah.Maftir] =
    Schedule.get(day, inHolyLand).morning.get.maftir.doFind(custom)

  "Shabbos Rosh Chodesh in Av and Elul" should "read the Rosh Chodesh maftir" in:
    var checked = 0
    for
      number <- years
      inHolyLand <- Seq(false, true)
      monthName <- Seq[Month.Name](Av, Elul)
    do
      val year = Year(number)
      val roshChodesh: Day = year.month(monthName).firstDay
      if roshChodesh.isShabbos then
        for custom <- Custom.values.filter(_ != Custom.Common) do
          val maftir = maftirOn(roshChodesh, inHolyLand, custom)
          withClue(s"$number $monthName holyLand=$inHolyLand $custom: ")(
            maftir.map(maftir => (
              maftir.book.toString,
              maftir.span.from.chapter, maftir.span.from.verse,
              maftir.span.to.chapter, maftir.span.to.verse
            )) shouldBe Some(expected))
        checked += 1

    // Rosh Chodesh Av and Elul fall on Shabbos often enough over two centuries
    checked should be > 20
