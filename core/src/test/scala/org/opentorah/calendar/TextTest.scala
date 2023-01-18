package org.opentorah.calendar

import org.opentorah.calendar.jewish.Jewish.{Day, Moment, Month, TimeVector, Year}
import org.opentorah.calendar.jewish.{Jewish, LeapYearsCycle, Moon, NewYear, Season, Sun}
import org.opentorah.calendar.roman.Gregorian
import org.opentorah.metadata.Language
import Week.Day.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

/**
  * Tests based on the statements from the text itself.
  */
class TextTest extends AnyFunSpec, Matchers:
  describe("Chapter 6") {
    describe("Law 2: Time Units") {
      it("hours") {
        Jewish.range(0) shouldBe 24
        Times.hoursPerDay shouldBe 24
        Times.hoursPerHalfDay shouldBe 12
      }
      it("parts") {
        Jewish.range(1) shouldBe 1080
        Times.partsPerHour shouldBe 1080
        Times.partsPerHour % 2 shouldBe 0
        Times.partsPerHour % 4 shouldBe 0
        Times.partsPerHour % 8 shouldBe 0
        Times.partsPerHour % 3 shouldBe 0
        Times.partsPerHour % 6 shouldBe 0
        Times.partsPerHour % 9 shouldBe 0
        Times.partsPerHour % 10 shouldBe 0
      }
      it("moments") { // KH 10:1
        Jewish.range(2) shouldBe 76
        Times.momentsPerPart shouldBe 76
      }
    }

    it("Law 3: mean lunar period") {
      Moon.meanLunarPeriod shouldBe TimeVector("29ᵈ12ʰ793ᵖ")
    }

    it("Law 4: year lengths") {
      LeapYearsCycle.normalYear shouldBe Moon.meanLunarPeriod*12
      LeapYearsCycle.normalYear shouldBe TimeVector("354ᵈ8ʰ876ᵖ")

      LeapYearsCycle.leapYear shouldBe Moon.meanLunarPeriod*13
      LeapYearsCycle.leapYear shouldBe TimeVector("383ᵈ21ʰ589ᵖ")

      // see also KH 9:1, 10:6
      Sun.Shmuel.yearLength shouldBe TimeVector("365ᵈ6ʰ")
      (Sun.Shmuel.yearLength - LeapYearsCycle.normalYear) shouldBe TimeVector("10ᵈ21ʰ204ᵖ")
    }

    describe("Law 5: Remainders for a Week") {
      it("month") {
        remainderForWeek(Moon.meanLunarPeriod     ) shouldBe TimeVector("1ᵈ12ʰ793ᵖ")
      }
      it("normal year") {
        remainderForWeek(LeapYearsCycle.normalYear) shouldBe TimeVector("4ᵈ 8ʰ876ᵖ")
      }
      it("leap year") {
        remainderForWeek(LeapYearsCycle.leapYear  ) shouldBe TimeVector("5ᵈ21ʰ589ᵖ")
      }
    }

    it("Law 7: molad Nisan example") {
      /*
      Rambam mentions Nisan conjunction on Sunday, 5 hours and 107 units parts after sunrise.
      He doesn't give the year, and there seems to be no such year...

      Koritz:
        [ז] אהק"ז, נ"א איזק"ז (רד"ש).
        מולד ניסן באחד בשבת 17 часов ו107 חלקים
        Один из комментаторов недоволен этими числами у Рамбама и меняет часы (17 вместо 5).
        Мой вопрос: решает ли таков изменение твою проблему.
       */
      // TODO Note: Rambam says 5 instead of 17:
//        for (yearNumber <- 4890.to(4966) /*1.to(6000)*/) { // Rambam: 1138–1204
//          val moladNisan = Year(yearNumber).month(Month.Nisan).newMoon
//          if (moladNisan.day.name == Rishon) //(moladNisan.parts == 107)
//            println(
//              moladNisan.day.toLanguageString(Language.English.toSpec) + " " +
//              moladNisan.to(Gregorian).toLanguageString(Language.English.toSpec) + " " +
//              moladNisan.day.name + " " +
//              s"${moladNisan.hours}h ${moladNisan.parts}p"
//            )
        (TimeVector("0ᵈ17ʰ107ᵖ") + Moon.meanLunarPeriod).time shouldBe TimeVector("0ᵈ5ʰ900ᵖ")
    }

    describe("Law 8: First New Moons") {
      val year1newMoon: Moment = Year(1).newMoon

      it("year 1") {
        year1newMoon.day.name shouldBe Sheni
        // see also KH 6:13
        year1newMoon.time shouldBe TimeVector("0ᵈ5ʰ204ᵖ") // BaHaRaD
      }

      it("year 2") {
        val year2newMoon: Moment = Year(2).newMoon
        year2newMoon.day.name shouldBe Shishi
        year2newMoon.time shouldBe TimeVector("0ᵈ14ʰ") // WeYaD 8:00am

        (year2newMoon - Moon.meanLunarPeriod*12) shouldBe year1newMoon
      }
    }

    it("Law 10: year of Shmuel") {
      // see also 9:1-2
      LeapYearsCycle.yearsInCycle shouldBe 19
      LeapYearsCycle.leapYearsInCycle shouldBe 7
      Sun.Shmuel.yearLength shouldBe TimeVector("365ᵈ6ʰ")
      LeapYearsCycle.cycleLength shouldBe (LeapYearsCycle.normalYear*12 + LeapYearsCycle.leapYear*7)
      (Sun.Shmuel.yearLength*LeapYearsCycle.yearsInCycle - LeapYearsCycle.cycleLength) shouldBe TimeVector("0ᵈ1ʰ485ᵖ")
      // KH 9:2
      Sun.Shmuel.seasonLength shouldBe TimeVector("91ᵈ7ʰ").halfHour
    }

    it("Law 11: leap years") {
      LeapYearsCycle.leapYears shouldBe Set(3, 6, 8, 11, 14, 17, 19)
    }

    it("Law 12: cycle remainder") {
      remainderForWeek(TimeVector("4ᵈ8ʰ876ᵖ")*12 + TimeVector("5ᵈ21ʰ589ᵖ")*7) shouldBe TimeVector("2ᵈ16ʰ595ᵖ")
    }
  }

  describe("Chapter 8") {
    it("Laws 7-8: year length for short years") {
      Year.shortNonLeapYearLength shouldBe 353
      Year.shortLeapYearLength shouldBe 383
    }

    it("Law 9: kind of the year") {
      var numberRegular: Int = 0
      var numberFull: Int = 0
      var numberShort: Int = 0
      for
        yearNumber <- 2 to 6000 // TODO remove magic constants
        year: Year = Year(yearNumber)
        if !year.isLeap
        firstDay: Day = year.firstDay
        nextYearFirstDay: Day = year.next.firstDay
        kind: Year.Kind = year.kind
      do
        if firstDay.is(Chamishi) then
          if nextYearFirstDay.is(Sheni) then
            numberRegular += 1
            kind shouldBe Year.Kind.Regular
          if nextYearFirstDay.is(Shlishi) then
            numberFull += 1
            kind shouldBe Year.Kind.Full
        if firstDay.is(Shabbos) then
          if nextYearFirstDay.is(Shlishi) then
            numberShort += 1
            kind shouldBe Year.Kind.Short

      // Verify that we saw some years with properties from the text
      assert(numberRegular > 0) // 1084 of them!
      assert(numberFull    > 0) // 198 of them!
      assert(numberShort   > 0) // 259 of them!
    }

    it("Law 10: year kind laws") {
      for yearNumber <- NewYear.delaysEnabledFromYear to 6000 do // TODO remove magic constants
        val year: Year = Year(yearNumber)
        val kind: Year.Kind = year.kind
        year.firstDay.name match
          case Shlishi  => assert(kind == Year.Kind.Regular)
          case Shabbos  => assert(kind != Year.Kind.Regular)
          case Sheni    => assert(kind != Year.Kind.Regular)
          case Chamishi => assert(kind != (if year.isLeap then Year.Kind.Regular else Year.Kind.Short))
          case _ => throw IllegalArgumentException()
    }
  }

  describe("Chapter 9") {
    it("Laws 3-4: first tkufas Nisan for Shmuel") {
      Moon.firstMoladNisan shouldBe Year(1).month(Month.Nisan).newMoon
      (Moon.firstMoladNisan - Sun.Shmuel.firstTkufasNisan) shouldBe TimeVector("7ᵈ9ʰ642ᵖ")
      Sun.Shmuel.firstTkufasNisan.day.name shouldBe Rvii
    }

    it("Laws 5-7: tkufos of 4930") {
      val year: Year = Year(4930)

      LeapYearsCycle.forYear(Jewish)(year) shouldBe YearsCycle.In(260, 9)

      def check(year: Year, season: Season, weekDay: Week.Day, time: TimeVector): Unit =
        val result: Moment = Sun.Shmuel.seasonForYear(season, year)
        result.day.name shouldBe weekDay
        result.time shouldBe time

      Sun.Shmuel.seasonForYear(Season.TkufasNisan, year).day shouldBe year.month(Month.Nisan).day(8)

      check(year  , Season.TkufasNisan  , Chamishi, TimeVector("0ᵈ 6ʰ")         )
      check(year  , Season.TkufasTammuz , Chamishi, TimeVector("0ᵈ13ʰ").halfHour)
      check(year  , Season.TkufasTishrei, Chamishi, TimeVector("0ᵈ21ʰ")         )
      check(year  , Season.TkufasTeves  , Shishi  , TimeVector("0ᵈ 4ʰ").halfHour)
      check(year+1, Season.TkufasNisan  , Shishi  , TimeVector("0ᵈ12ʰ")         )
    }
  }

  describe("Chapter 10") {
    it("Laws 1-2: year of Rav Ada") {
      Sun.RavAda.yearLength shouldBe TimeVector("365ᵈ5ʰ997ᵖ48ᵐ")

      (Sun.RavAda.yearLength - LeapYearsCycle.normalYear) shouldBe TimeVector("10ᵈ21ʰ121ᵖ48ᵐ")

      (Sun.RavAda.yearLength*LeapYearsCycle.yearsInCycle - LeapYearsCycle.cycleLength) shouldBe TimeVector.zero

      // KH 10:2
      Sun.RavAda.seasonLength shouldBe TimeVector("91ᵈ7ʰ519ᵖ31ᵐ")
    }

    it("Law 3: first tkufas Nisan for RavAda") {
      (Moon.firstMoladNisan - Sun.RavAda.firstTkufasNisan) shouldBe TimeVector("0ᵈ9ʰ642ᵖ")

      Sun.RavAda.firstTkufasNisan.day.name shouldBe Rvii
    }
  }

  private val week: TimeVector = TimeVector().days(Week.length) // TODO merge into Week?

  private def remainderForWeek(ofWhat: TimeVector): TimeVector =
    ofWhat - (week * (ofWhat.toRational / week.toRational).whole)
