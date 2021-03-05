package org.opentorah.calendar

import org.opentorah.calendar.jewish.Jewish.{Day, Moment, Month, TimeVector, Year}
import org.opentorah.calendar.jewish.{Jewish, LeapYearsCycle, Moon, NewYear, Season, Sun}
import Week.Day._
import org.opentorah.metadata.Language
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

/**
  * Tests based on the statements from the text itself.
  */
class TextTest extends AnyFunSpec with Matchers {
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

    describe("Law 3") {
      it("mean lunar period") {
        Moon.meanLunarPeriod shouldBe TimeVector().days(29).hours(12).parts(793)
      }
    }

    describe("Law 4") {
      it("year lengths") {
        LeapYearsCycle.normalYear shouldBe Moon.meanLunarPeriod*12
        LeapYearsCycle.normalYear shouldBe TimeVector().days(354).hours( 8).parts(876)

        LeapYearsCycle.leapYear shouldBe Moon.meanLunarPeriod*13
        LeapYearsCycle.leapYear shouldBe TimeVector().days(383).hours(21).parts(589)

        // see also KH 9:1, 10:6
        Sun.Shmuel.yearLength shouldBe TimeVector().days(365).hours(6)
        (Sun.Shmuel.yearLength - LeapYearsCycle.normalYear) shouldBe TimeVector().days(10).hours(21).parts(204)
      }
    }

    describe("Law 5: Remainders for a Week") {
      it("month") {
        remainderForWeek(Moon.meanLunarPeriod     ) shouldBe TimeVector().days(1).hours(12).parts(793)
      }
      it("normal year") {
        remainderForWeek(LeapYearsCycle.normalYear) shouldBe TimeVector().days(4).hours( 8).parts(876)
      }
      it("leap year") {
        remainderForWeek(LeapYearsCycle.leapYear  ) shouldBe TimeVector().days(5).hours(21).parts(589)
      }
    }

    describe("Law 7") {
      /*
      Rambam mentions Nisan conjunction on Sunday, 5 hours and 107 units parts after sunrise.
      He doesn't give the year, and there seems to be no such year...

      Koritz:
        [ז] אהק"ז, נ"א איזק"ז (רד"ש).
        מולד ניסן באחד בשבת 17 часов ו107 חלקים
        Один из комментаторов недоволен этими числами у Рамбама и меняет часы (17 вместо 5).
        Мой вопрос: решает ли таков изменение твою проблему.
       */
      it("molad Nisan example") { // TODO Note: Rambam says 5 instead of 17:
//        for (yearNumber <- 1.to(6000)) {
//          val moladNisan = Year(yearNumber).month(Month.Nisan).newMoon
//          if (moladNisan.parts == 107) println(
//            moladNisan.day.toLanguageString(Language.English.toSpec) + " " +
//            moladNisan.day.name + " " +
//            s"${moladNisan.hours}h ${moladNisan.parts}p"
//          )
//        }
        (TimeVector().hours(17).parts(107) + Moon.meanLunarPeriod).time shouldBe
          TimeVector().hours( 5).parts(900)
      }
    }

    describe("Law 8: First New Moons") {
      val year1newMoon: Moment = Year(1).newMoon

      it("year 1") {
        year1newMoon.day.name shouldBe Sheni
        // see also KH 6:13
        year1newMoon.time shouldBe TimeVector().hours(5).parts(204) // BaHaRaD
      }

      it("year 2") {
        val year2newMoon: Moment = Year(2).newMoon
        year2newMoon.day.name shouldBe Shishi
        year2newMoon.time shouldBe TimeVector().hours(14) // WeYaD 8:00am

        (year2newMoon - Moon.meanLunarPeriod*12) shouldBe year1newMoon
      }
    }

    describe("Law 10") {
      it("year of Shmuel") {
        // see also 9:1-2
        LeapYearsCycle.yearsInCycle shouldBe 19
        LeapYearsCycle.leapYearsInCycle shouldBe 7
        Sun.Shmuel.yearLength shouldBe TimeVector().days(365).hours(6)
        LeapYearsCycle.cycleLength shouldBe (LeapYearsCycle.normalYear*12 + LeapYearsCycle.leapYear*7)
        (Sun.Shmuel.yearLength*LeapYearsCycle.yearsInCycle - LeapYearsCycle.cycleLength) shouldBe
          TimeVector().hours(1).parts(485)
        // KH 9:2
        Sun.Shmuel.seasonLength shouldBe TimeVector().days(91).hours(7).halfHour
      }
    }

    describe("Law 11") {
      it("leap years") {
        LeapYearsCycle.leapYears shouldBe Set(3, 6, 8, 11, 14, 17, 19)
      }
    }

    describe("Law 12") {
      it("cycle remainder") {
        remainderForWeek(TimeVector().days(4).hours( 8).parts(876)*12 +
          TimeVector().days(5).hours(21).parts(589)* 7) shouldBe
          TimeVector().days(2).hours(16).parts(595)
      }
    }
  }

  describe("Chapter 8") {
    describe("Laws 7-8") {
      it("year length for short years") {
        Year.shortNonLeapYearLength shouldBe 353
        Year.shortLeapYearLength shouldBe 383
      }
    }

    describe("Law 9") {
      it("kind of the year") {
        var numberRegular: Int = 0
        var numberFull: Int = 0
        var numberShort: Int = 0
        for {
          yearNumber <- 2 to 6000 // TODO remove magic constants
          year: Year = Year(yearNumber)
          if !year.isLeap
          firstDay: Day = year.firstDay
          nextYearFirstDay: Day = year.next.firstDay
          kind: Year.Kind = year.kind
        } {
          if (firstDay.is(Chamishi)) {
            if (nextYearFirstDay.is(Sheni)) {
              numberRegular += 1
              kind shouldBe Year.Kind.Regular
            }
            if (nextYearFirstDay.is(Shlishi)) {
              numberFull += 1
              kind shouldBe Year.Kind.Full
            }
          }
          if (firstDay.is(Shabbos)) {
            if (nextYearFirstDay.is(Shlishi)) {
              numberShort += 1
              kind shouldBe Year.Kind.Short
            }
          }
        }

        // Verify that we saw some years with properties from the text
        assert(numberRegular > 0) // 1084 of them!
        assert(numberFull    > 0) // 198 of them!
        assert(numberShort   > 0) // 259 of them!
      }
    }

    describe("Law 10") {
      it("year kind laws") {
        for (yearNumber <- NewYear.delaysEnabledFromYear to 6000) { // TODO remove magic constants
          val year: Year = Year(yearNumber)
          val kind: Year.Kind = year.kind
          year.firstDay.name match {
            case Shlishi  => assert(kind == Year.Kind.Regular)
            case Shabbos  => assert(kind != Year.Kind.Regular)
            case Sheni    => assert(kind != Year.Kind.Regular)
            case Chamishi => assert(kind != (if (year.isLeap) Year.Kind.Regular else Year.Kind.Short))
            case _ => throw new IllegalArgumentException
          }
        }
      }
    }
  }

  describe("Chapter 9") {
    describe("Laws 3-4") {
      it("first tkufas Nisan for Shmuel") {
        Moon.firstMoladNisan shouldBe Year(1).month(Month.Nisan).newMoon
        (Moon.firstMoladNisan - Sun.Shmuel.firstTkufasNisan) shouldBe
          TimeVector().days(7).hours(9).parts(642)
        Sun.Shmuel.firstTkufasNisan.day.name shouldBe Rvii
      }
    }

    describe("Laws 5-7") {
      it("tkufos of 4930") {
        val year: Year = Year(4930)

        LeapYearsCycle.forYear(year) shouldBe YearsCycle.In(260, 9)

        val tkufasNisan: Moment = Sun.Shmuel.seasonForYear(Season.TkufasNisan, year)
        tkufasNisan.day.name shouldBe Chamishi
        tkufasNisan.time shouldBe TimeVector().hours(6)
        tkufasNisan.day shouldBe year.month(Month.Nisan).day(8)

        val tkufasTammuz: Moment = Sun.Shmuel.seasonForYear(Season.TkufasTammuz, year)
        tkufasTammuz.day.name shouldBe Chamishi
        tkufasTammuz.time shouldBe TimeVector().hours(13).halfHour

        val tkufasTishrei: Moment = Sun.Shmuel.seasonForYear(Season.TkufasTishrei, year)
        tkufasTishrei.day.name shouldBe Chamishi
        tkufasTishrei.time shouldBe TimeVector().hours(21)

        val tkufasTeves: Moment = Sun.Shmuel.seasonForYear(Season.TkufasTeves, year)
        tkufasTeves.day.name shouldBe Shishi
        tkufasTeves.time shouldBe TimeVector().hours(4).halfHour

        val nextTkufasNisan: Moment = Sun.Shmuel.seasonForYear(Season.TkufasNisan, year+1)
        nextTkufasNisan.day.name shouldBe Shishi
        nextTkufasNisan.time shouldBe TimeVector().hours(12)
      }
    }
  }

  describe("Chapter 10") {
    describe("Laws 1-2") {
      it("year of RavAda") {
        Sun.RavAda.yearLength shouldBe
          TimeVector().days(365).hours(5 ).parts(997).moments(48)

        (Sun.RavAda.yearLength - LeapYearsCycle.normalYear) shouldBe
          TimeVector().days( 10).hours(21).parts(121).moments(48)

        (Sun.RavAda.yearLength*LeapYearsCycle.yearsInCycle - LeapYearsCycle.cycleLength) shouldBe TimeVector.zero

        // KH 10:2
        Sun.RavAda.seasonLength shouldBe
          TimeVector().days(91).hours(7).parts(519).moments(31)
      }
    }

    describe("Law 3") {
      it("first tkufas Nisan for RavAda") {
        (Moon.firstMoladNisan - Sun.RavAda.firstTkufasNisan) shouldBe
          TimeVector().hours(9).parts(642)

        Sun.RavAda.firstTkufasNisan.day.name shouldBe Rvii
      }
    }
  }

  private val week: TimeVector = TimeVector().days(Week.length) // TODO merge into Week?

  private def remainderForWeek(ofWhat: TimeVector): TimeVector =
    ofWhat - (week * (ofWhat.toRational / week.toRational).whole)
}
