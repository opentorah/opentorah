package org.opentorah.calendar

import org.opentorah.calendar.jewish.Jewish.TimeVector
import org.opentorah.calendar.jewish.{LeapYearsCycle, Sun}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class LeapYearsCycleTest extends AnyFlatSpec, Matchers:

  val beforeAdar: TimeVector = LeapYearsCycle.solarMonthOverLunar*5
  val afterAdar: TimeVector = LeapYearsCycle.solarMonthOverLunar*7

  val additionalMonth: TimeVector = TimeVector("30ᵈ") // inserted Adar I is always 30 days

  def beforeAdar(year: Int): TimeVector =
    def sum(what: Seq[TimeVector]): TimeVector =
      if what.isEmpty then TimeVector() else what.reduce((a, b) => a+b)

    val previously = for
      i <- 1.until(year)
      isLeap = LeapYearsCycle.leapYears.contains(i)
      solarOverLunar = LeapYearsCycle.solarMonthOverLunar*LeapYearsCycle.yearLengthInMonths(isLeap)
    yield
      if isLeap then solarOverLunar - additionalMonth else solarOverLunar

    sum(previously) + beforeAdar

  // Pirush, KH 1:2
  // Note: apparently, Pirush calculated Sun.Shmuel.monthLength and LeapYearsCycle.solarMonthOverLunar
  "Solar over lunar" should "be correct" in {
    Sun.Shmuel.monthLength shouldBe TimeVector("30ᵈ10ʰ540ᵖ")

    LeapYearsCycle.solarMonthOverLunar shouldBe TimeVector("0ᵈ21ʰ827ᵖ")

    // before Adar year 1: 4d12h895p
    // before Adar year 2: 15d10h19p
    // before Adar year 3: 26d7h223p
    val beforeAdarYear3: TimeVector = LeapYearsCycle.solarYearOverLunar*2 + beforeAdar
    beforeAdarYear3 shouldBe TimeVector("26ᵈ7ʰ223ᵖ") // "almost 27 days"

    // before Adar year 4: 8d2h174p
    // before Adar year 5: 18d23h378p
    // before Adar year 6: 29d20h582p
    // Note: Pirush's "remainder for the 3rd year" means "remainder for the remainder of the 3rd year", the 8 months
    // starting with the inserted Adar I
    val beforeAdarYear6: TimeVector = LeapYearsCycle.solarMonthOverLunar*8 + LeapYearsCycle.solarYearOverLunar*2 + beforeAdar
    beforeAdarYear6 shouldBe TimeVector("33ᵈ13ʰ359ᵖ") // "about 33 days"

    // before Adar year 7: 11d15h533p
    // before Adar year 8: 22d12h737p
    // Note: here Pirush takes only 7 months of the 6th year, taking beforeAdarYear6 as equal in length to the inserted month?!
    val beforeAdarYear8 = LeapYearsCycle.solarMonthOverLunar*7 + LeapYearsCycle.solarYearOverLunar*1 + beforeAdar
    beforeAdarYear8 shouldBe TimeVector("21ᵈ18ʰ408ᵖ") // "about 22 days"

    // before Adar year 9: 4d7h688p
    // before Adar year 10: 15d4h892p
    // before Adar year 11: 26d2h16p
    val beforeAdarYear11uncorrected = LeapYearsCycle.solarMonthOverLunar*8 + LeapYearsCycle.solarYearOverLunar*2 + beforeAdar
    beforeAdarYear11uncorrected shouldBe TimeVector("33ᵈ13ʰ359ᵖ") // "about 33 days"
    val overcompensatedYear6 = additionalMonth - beforeAdarYear8
    overcompensatedYear6 shouldBe TimeVector("8ᵈ5ʰ672ᵖ") // "8 days we over-added in the 6th year"
    val beforeAdarYear11 = beforeAdarYear11uncorrected - overcompensatedYear6
    beforeAdarYear11 shouldBe TimeVector("25ᵈ7ʰ767ᵖ") // "about 25 days"

    // before Adar year 12: 7d20h1047p
    // before Adar year 13: 18d18h171p
    // before Adar year 14: 29d15h375p
    val beforeAdarYear14uncorrected = LeapYearsCycle.solarMonthOverLunar * 8 + LeapYearsCycle.solarYearOverLunar * 2 + beforeAdar
    beforeAdarYear14uncorrected shouldBe TimeVector("33ᵈ13ʰ359ᵖ") // "about 33 days"
    val overcompensatedYear11 = additionalMonth - beforeAdarYear11
    overcompensatedYear11 shouldBe TimeVector("4ᵈ16ʰ313ᵖ") // "5 days"
    val beforeAdarYear14 = beforeAdarYear14uncorrected - overcompensatedYear11
    beforeAdarYear14 shouldBe TimeVector("28ᵈ21ʰ46ᵖ") // "28 days"

    // before Adar year 15: 11d10h326p
    // before Adar year 16: 22d7h530p
    // before Adar year 17: 33d4h734p
    val beforeAdarYear17uncorrected = LeapYearsCycle.solarMonthOverLunar * 8 + LeapYearsCycle.solarYearOverLunar * 2 + beforeAdar
    beforeAdarYear17uncorrected shouldBe TimeVector("33ᵈ13ʰ359ᵖ") // "33 days"
    val overcompensatedYear14 = additionalMonth - beforeAdarYear14
    overcompensatedYear14 shouldBe TimeVector("1ᵈ2ʰ1034ᵖ") // "2 days"?!
    val beforeAdarYear17 = beforeAdarYear17uncorrected - overcompensatedYear14
    beforeAdarYear17 shouldBe TimeVector("32ᵈ10ʰ405ᵖ") // "31 days"?!

    // before Adar year 18: 14d23h685p
    // before Adar year 19: 25d20h889p
    // Note: "from it" - including or not? Or does Pirush correct for overcompensation here too?
    val beforeEndOfCycle = LeapYearsCycle.solarMonthOverLunar * 7 + LeapYearsCycle.solarYearOverLunar * 2
    beforeEndOfCycle shouldBe TimeVector("28ᵈ2ʰ797ᵖ") // "about 27 days"?

//    for year <- 1.to(LeapYearsCycle.yearsInCycle) do println(s"before Adar year $year: ${beforeAdar(year)}")
  }
