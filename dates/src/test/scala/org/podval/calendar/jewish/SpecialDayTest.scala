package org.podval.calendar.jewish

import org.scalatest.{FlatSpec, Matchers}
import Jewish.Day
import Day.Name._
import SpecialDay._

class SpecialDayTest extends FlatSpec with Matchers {

  // Shulchan Aruch, Orach Chaim, 428:1; Rambam TODO
  "Festivals" should "not fall on the proscribed days" in {
    (1 to 6000) foreach { number =>
      val year = Jewish.Year(number)

      def notOn(specialDay: SpecialDay, days: Day.Name*): Unit =
        days contains specialDay(year).name shouldBe false

      notOn(RoshHashanah, Rishon, Rvii, Shishi)
      notOn(YomKippur, Shlishi, Rishon, Shishi)
      notOn(Purim, Shabbos, Sheni, Rvii)
      notOn(Pesach, Sheni, Rvii, Shishi)
      notOn(Shavuot, Shlishi, Chamishi, Shabbos)
      notOn(HoshanahRabbah, Shlishi, Chamishi, Shabbos)
      notOn(Hanukkah, Shlishi)
      notOn(FastOfEster, Rishon, Shlishi, Shishi)
      notOn(FastOfTammuz, Sheni, Rvii, Shishi)
      notOn(TishaBav, Sheni, Rvii, Shishi)

      def sameDay(a: SpecialDay, b: SpecialDay): Unit =
        a(year).name shouldBe b(year).name

      sameDay(Purim, LagBaOmer)
      // TODO This is wrong; see Taz
      // sameDay(Hanukkah, Shavuot)
    }
  }

  // TODO Shulchan Aruch, Orach Chaim, 428:2; Rambam TODO
  // TODO Shulchan Aruch, Orach Chaim, 428:3; Rambam TODO
}
