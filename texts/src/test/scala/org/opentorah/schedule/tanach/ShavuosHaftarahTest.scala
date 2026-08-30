package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.jewish.SpecialDay.{Shavuos1, Shavuos2}
import org.opentorah.texts.tanach.Custom
import Jewish.Year
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class ShavuosHaftarahTest extends AnyFlatSpec, Matchers:

  private def haftarah(day: Jewish.Day, custom: Custom): String =
    Schedule.get(day, inHolyLand = false).morning.get
      .haftarah.doFind(custom).map(_.toString).getOrElse("-")

  private val years = 5700 to 5800

  "Shavuos" should "read Ezekiel 1 and the single verse 3:12" in:
    // 3:12 is one verse -- ברוך כבוד ה' ממקומו -- not the rest of the chapter.
    for number <- years do
      val day = Shavuos1.date(Year(number))
      withClue(s"$number: ") {
        haftarah(day, Custom.Ashkenaz) should include("3:12")
        haftarah(day, Custom.Ashkenaz) should not include "3:12-"
        // Teiman reads further into chapter 1, and the same single verse
        haftarah(day, Custom.Teiman) should include("1:1-2:2")
      }

  "the second day of Shavuos" should "follow Sefard for Italki" in:
    // Ashkenaz begins at Habakkuk 3:1; Sefard at 2:20, and Italki with them.
    for number <- years do
      val day = Shavuos2.date(Year(number))
      withClue(s"$number: ") {
        haftarah(day, Custom.Ashkenaz) should include("3:1-19")
        haftarah(day, Custom.Sefard) should include("2:20-3:19")
        haftarah(day, Custom.Italki) should include("2:20-3:19")
      }
