package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.jewish.SpecialDay
import org.opentorah.calendar.jewish.SpecialDay.*
import Jewish.{Day, Year}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * How many aliyot each kind of day is read with. A Shabbos is seven whatever
 * else the day is, so every count below is the weekday one and gives way to
 * seven when the day falls on Shabbos.
 */
final class AliyotCountTest extends AnyFlatSpec, Matchers:

  private val years: Range = 5700 to 5900

  private def aliyot(day: Day, inHolyLand: Boolean): Set[Int] =
    // Single day on purpose: a wider window would also build the days around
    // it, and a public fast in that window cannot be built at all -- see
    // getWeekdayMorningReading, which has no case for the fasts.
    Schedule.get(day, inHolyLand).morning.get.torah.customs.values.map(_.length).toSet

  /** Assert the count for one special day across the whole range. */
  private def check(
    days: Seq[SpecialDay],
    onWeekday: Int,
    inHolyLand: Boolean,
    label: String
  ): (Int, Int) =
    var weekdays = 0
    var shabbosos = 0
    for
      number <- years
      special <- days
    do
      val day = special.date(Year(number))
      val expected = if day.isShabbos then 7 else onWeekday
      withClue(s"$label ${special.getClass.getSimpleName} $number " +
               s"shabbos=${day.isShabbos} holyLand=$inHolyLand: ") {
        aliyot(day, inHolyLand) shouldBe Set(expected)
      }
      if day.isShabbos then shabbosos += 1 else weekdays += 1
    (weekdays, shabbosos)

  "Yom Kippur" should "be six aliyot, or seven on Shabbos" in:
    val (weekdays, shabbosos) = check(Seq(YomKippur), 6, inHolyLand = false, "Yom Kippur")
    // both cases must occur, or the test proves nothing
    weekdays should be > 0
    shabbosos should be > 0

  "Yomim tovim" should "be five aliyot, or seven on Shabbos" in:
    val (weekdays, shabbosos) = check(
      Seq(RoshHashanah1, RoshHashanah2, Succos1, Succos2, SheminiAtzeres,
          Pesach1, Pesach2, Pesach7, Pesach8, Shavuos1, Shavuos2),
      5, inHolyLand = false, "Yom Tov")
    weekdays should be > 0
    shabbosos should be > 0

  "Simchas Torah" should "be seven aliyot even on a weekday" in:
    val (weekdays, shabbosos) = check(Seq(SimchasTorah), 7, inHolyLand = false, "Simchas Torah")
    weekdays should be > 0
    // 23 Tishrei is never a Shabbos, since Rosh Hashanah is never a Sunday
    shabbosos shouldBe 0
    check(Seq(SheminiAtzeresAndSimchasTorahInHolyLand), 7, inHolyLand = true, "Simchas Torah (EY)")

  "Chol HaMoed" should "be four aliyot, or seven on Shabbos" in:
    val (weekdays, shabbosos) = check(
      Seq(SuccosIntermediate1, SuccosIntermediate2, SuccosIntermediate3,
          SuccosIntermediate4, HoshanahRabbah,
          PesachIntermediate1, PesachIntermediate2, PesachIntermediate3,
          PesachIntermediate4),
      4, inHolyLand = false, "Chol HaMoed")
    weekdays should be > 0
    shabbosos should be > 0

  it should "be four aliyot in Eretz Yisrael too, on Shabbos and off" in:
    val (weekdays, shabbosos) = check(
      Seq(SuccosIntermediate1InHolyLand, SuccosIntermediate2InHolyLand,
          SuccosIntermediate3InHolyLand, SuccosIntermediate4InHolyLand,
          SuccosIntermediate5InHolyLand, HoshanahRabbahInHolyLand,
          PesachIntermediate1InHolyLand, PesachIntermediate2InHolyLand,
          PesachIntermediate3InHolyLand, PesachIntermediate4InHolyLand,
          PesachIntermediate5InHolyLand),
      4, inHolyLand = true, "Chol HaMoed (EY)")
    weekdays should be > 0
    shabbosos should be > 0

  "Rabbinic festivals" should "be three aliyot, four with Rosh Chodesh, seven on Shabbos" in:
    var plain = 0        // Chanukah or Purim on an ordinary weekday: three
    var withRoshChodesh = 0   // Chanukah and Rosh Chodesh Teves: four
    var onShabbos = 0         // Chanukah on Shabbos: seven
    var roshChodeshShabbos = 0  // Chanukah and Rosh Chodesh on Shabbos: seven
    for
      number <- years
      special <- Seq(Chanukah1, Chanukah2, Chanukah3, Chanukah4, Chanukah5,
                     Chanukah6, Chanukah7, Chanukah8, Purim)
    do
      val day = special.date(Year(number))
      val expected =
        if day.isShabbos then 7
        else if day.isRoshChodesh then 4
        else 3
      withClue(s"$number ${special.getClass.getSimpleName} " +
               s"shabbos=${day.isShabbos} roshChodesh=${day.isRoshChodesh}: ")(
        aliyot(day, inHolyLand = false) shouldBe Set(expected))
      if day.isShabbos && day.isRoshChodesh then roshChodeshShabbos += 1
      else if day.isShabbos then onShabbos += 1
      else if day.isRoshChodesh then withRoshChodesh += 1
      else plain += 1
    // all four combinations must actually occur
    plain should be > 0
    withRoshChodesh should be > 0
    onShabbos should be > 0
    roshChodeshShabbos should be > 0

  "Rosh Chodesh" should "be four aliyot, or seven on Shabbos" in:
    var weekdays = 0
    var shabbosos = 0
    for
      number <- 5780 to 5800
      inHolyLand <- Seq(false, true)
    do
      val year = Year(number)
      var day: Day = year.firstDay
      while day.number <= year.lastDay.number do
        // 1 Tishrei is Rosh Chodesh but is read as Rosh Hashanah; Chanukah's
        // Rosh Chodesh is ordinary Rosh Chodesh and is included.
        if day.isRoshChodesh && day.number != RoshHashanah1.date(year).number then
          val expected = if day.isShabbos then 7 else 4
          withClue(s"$number-${day.month.numberInYear}-${day.numberInMonth} " +
                   s"shabbos=${day.isShabbos} holyLand=$inHolyLand: ")(
            aliyot(day, inHolyLand) shouldBe Set(expected))
          if day.isShabbos then shabbosos += 1 else weekdays += 1
        day = day.next
    weekdays should be > 0
    shabbosos should be > 0
