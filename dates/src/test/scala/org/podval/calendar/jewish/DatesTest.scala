package org.podval.calendar.jewish

import org.scalatest.{FlatSpec, Matchers}
import Jewish.{Day, Month, Year}
import Jewish.TimeVector
import Jewish.Day.Name._
import Jewish.Month.Name._
import SpecialDay._
import org.podval.calendar.dates.Calendar
import org.podval.calendar.gregorian.Gregorian
import Gregorian.Month.Name._


final class DatesTest extends FlatSpec with Matchers {

  "Known dates" should "have correct day of the week" in {
    Day(5772, Marheshvan, 24).name shouldBe Day.Name.Sheni
  }

  // TODO redo with data sources
  "Conversions from date to days and back" should "end where they started" in {
    def test(yearNumber: Int, monthName: Month.Name, dayNumber: Int): Unit = {
      val year = Year(yearNumber)
      year.number shouldBe yearNumber

      val month = year.month(monthName)
      month.name shouldBe monthName

      val day = month.day(dayNumber)

      day.year shouldBe year
      day.month shouldBe month
      day.numberInMonth shouldBe dayNumber
    }

    test(1   , Tishrei,  1)
    test(2   , Tishrei,  1)
    test(5768, AdarII , 28)
    test(5769, Nisan  , 14)
  }

  // TODO redo with data sources
  "New moons from the printed tables" should "calculate correctly" in {
    // year and month for the molad; jewish date; georgian date; georgian time
    newMoon(5769, Tishrei   , Shlishi ,  5769, Tishrei,  1,  2008, September, 30,  1, 58, 13)
    newMoon(5769, Marheshvan, Rvii    ,  5769, Tishrei, 30,  2008, October  , 29, 14, 42, 14)
    newMoon(5769, Kislev    , Shishi  ,  5769, Kislev ,  1,  2008, November , 28,  3, 26, 15)

    newMoon(5771, Tishrei   , Chamishi,  5771, Tishrei,  1,  2010, September,  8, 19, 36,  1)

    newMoon(5772, Tishrei   , Shlishi ,  5771, Elul   , 28,  2011, September, 27, 17,  8, 14)
    newMoon(5772, Marheshvan, Chamishi,  5772, Tishrei, 29,  2011, October  , 27,  5, 52, 15)

    newMoon(5773, Tishrei   , Rishon  ,  5772, Elul   , 29,  2012, September, 16,  1, 57,  8)
    newMoon(5773, Adar      , Rishon  ,  5773, Shvat  , 30,  2013, February , 10, 17, 37, 13)
    newMoon(5773, Nisan     , Shlishi ,  5773, Nisan  ,  1,  2013, March    , 12,  6, 21, 14)
  }

  private def newMoon(
    moladYear: Int, moladMonth: Jewish.MonthName, dayOfWeek: Jewish.DayName,
    year: Int, month: Jewish.MonthName, day: Int,
    yearG: Int, monthG: Gregorian.MonthName, dayG: Int,
    hours: Int, minutes: Int, parts: Int)
  {
    val dayJ = Jewish.Day(year, month, day)
    val dateG = Gregorian.Day(yearG, monthG, dayG).toMoment.
      hours(hours).minutes(minutes).partsWithoutMinutes(parts)
    val molad = Jewish.Year(moladYear).month(moladMonth).newMoon

    molad.day.name shouldBe dayOfWeek
    molad.day shouldBe dayJ

    Calendar.toJewish(dateG).day shouldBe dayJ
    Calendar.fromJewish(molad) shouldBe dateG
  }

  // TODO redo with data sources
  "Moment components" should "be correct" in {
    def test(days: Int, hours: Int, parts: Int): Unit = {
      val moment = TimeVector().days(days).hours(hours).parts(parts)

      moment.days shouldBe days
      moment.hours shouldBe hours
      moment.parts shouldBe parts
    }

    test( 0, 18,   0)
    test( 0,  9, 204)
    test( 0, 15, 589)
    test(29, 12, 793)
    test( 1,  5, 204)
  }

  private val years = (1 to 6000) map (Year(_))

  "Jewish year" should "be the year of the month retrieved from it" in {
    for (year <- years; month <- 1 to year.lengthInMonths)
      year.month(month).year shouldBe year
  }

  // Shulchan Aruch, Orach Chaim, 428:1
  "Festivals" should "not fall on the proscribed days" in {
    years foreach { year =>
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

  "Jewish Year" should "have correct character" in {
    years foreach { year =>
      val (isLeap, kind) = year.character
      val yearType = (if (isLeap) YearType.leap else YearType.nonLeap).find { yearType =>
        (yearType.roshHashanah == RoshHashanah(year).name) && (yearType.kind == kind)
      }
      Pesach(year).name shouldBe yearType.get.pesach
    }
  }
}

final case class YearType(roshHashanah: Day.Name, kind: Year.Kind, pesach: Day.Name)

// This table of unknown origin was submitted by @michaelko58.
// It lists all occurring year types:
//   day of the week of Rosh Hashanah
//   year kind (short, regular, full);
//   day of the week of Pesach.
object YearType {
  val nonLeap: Seq[YearType] = Seq("בחג" ,"בשה" ,"גכה" ,"הכז" ,"השא" ,"זחא", "זשג").map(apply)
  val leap: Seq[YearType] = Seq("בחה", "בשז", "גכז", "החא", "השג", "זחג", "זשה").map(apply)

  def apply(value: String): YearType = {
    require(value.length == 3)

    def dayOfTheWeek(char: Char): Day.Name = char match {
      case 'א' => Day.Name.Rishon
      case 'ב' => Day.Name.Sheni
      case 'ג' => Day.Name.Shlishi
      case 'ה' => Day.Name.Chamishi
      case 'ז' => Day.Name.Shabbos
    }

    new YearType(
      roshHashanah = dayOfTheWeek(value.charAt(0)),
      kind = value.charAt(1) match {
        case 'ח' => Year.Kind.Short
        case 'כ' => Year.Kind.Regular
        case 'ש' => Year.Kind.Full
      },
      pesach = dayOfTheWeek(value.charAt(2))
    )
  }
}
