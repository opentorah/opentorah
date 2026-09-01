package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish.Year
import org.opentorah.calendar.jewish.SpecialDay
import org.opentorah.texts.tanach.{Custom, SpecialReadings, Tanach}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Simchas Torah is read at night as well, by Ashkenaz and not by all of them.
 * The customs that do not read are given None rather than left out of the map,
 * so "reads nothing at night" is an answer and not a gap.
 */
final class SimchasTorahNightTest extends AnyFlatSpec, Matchers:

  private def night(year: Int) =
    Schedule.get(SpecialDay.SimchasTorah.date(Year(year)), inHolyLand = false).evening

  "the night of Simchas Torah" should "be read in the Diaspora" in:
    night(5787) should not be None

  it should "give Ashkenaz the first five aliyot of Vezos Haberachah" in:
    val reading = night(5787).get.torah.doFind(Custom.Ashkenaz)
    reading.map(_.spans.length) shouldBe Some(5)
    reading.get.spans.head.book shouldBe Tanach.Chumash.Deuteronomy

  it should "give the customs that do not read it None, not a missing entry" in:
    val evening = night(5787).get.torah
    for custom <- Custom.all.filterNot(_.isUnder(Custom.Ashkenaz)) do
      withClue(s"$custom: ")(evening.doFind(custom) shouldBe None)

  "the shorter practice" should "be said in the note, not left to be discovered" in:
    val note = night(5787).get.note
    note should include ("Ashkenaz")
    note should include ("first three")
    note should include ("Nitei Gavriel")

  "no other day" should "have a night reading" in:
    val year = Year(5787)
    var found = 0
    var day = year.firstDay
    while day <= year.lastDay do
      if Schedule.get(day, inHolyLand = false).evening.isDefined then found += 1
      day = day + 1
    found shouldBe 1
