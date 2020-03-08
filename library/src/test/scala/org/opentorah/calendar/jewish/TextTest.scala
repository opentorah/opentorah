package org.opentorah.calendar.jewish

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.opentorah.calendar.times.Times.{hoursPerDay, hoursPerHalfDay, momentsPerPart, partsPerHour}
import Jewish.{Day, Month, TimeVector, Year, range, week}
import LeapYearsCycle.{leapYear, normalYear}
import Moon.meanLunarPeriod
import Sun.{RavAda, Shmuel}
import org.opentorah.calendar.dates.YearsCycle

/**
  * Tests based on the statements from the text itself.
  */
class TextTest extends AnyFunSpec with Matchers {
  describe("Chapter 6") {
    describe("Law 2: Time Units") {
      it("hours") {
        hoursPerDay shouldBe 24
        range(0) shouldBe 24
        hoursPerHalfDay shouldBe 12
      }
      it("parts") {
        partsPerHour shouldBe 1080
        range(1) shouldBe 1080
        partsPerHour % 2 shouldBe 0
        partsPerHour % 4 shouldBe 0
        partsPerHour % 8 shouldBe 0
        partsPerHour % 3 shouldBe 0
        partsPerHour % 6 shouldBe 0
        partsPerHour % 9 shouldBe 0
        partsPerHour % 10 shouldBe 0
      }
      it("moments") { // KH 10:1
        momentsPerPart shouldBe 76
        range(2) shouldBe 76
      }
    }

    describe("Law 3") {
      it("mean lunar period") {
        meanLunarPeriod shouldBe TimeVector().days(29).hours(12).parts(793)
      }
    }

    describe("Law 4") {
      it("year lengths") {
        normalYear shouldBe meanLunarPeriod*12
        normalYear shouldBe TimeVector().days(354).hours( 8).parts(876)

        leapYear shouldBe meanLunarPeriod*13
        leapYear shouldBe TimeVector().days(383).hours(21).parts(589)

        // see also KH 9:1, 10:6
        Shmuel.yearLength shouldBe TimeVector().days(365).hours(6)
        (Shmuel.yearLength - normalYear) shouldBe TimeVector().days(10).hours(21).parts(204)
      }
    }

    describe("Law 5: Reminders for a Week") {
      it("month") {
        reminderForWeek(meanLunarPeriod) shouldBe TimeVector().days(1).hours(12).parts(793)
      }
      it("normal year") {
        reminderForWeek(normalYear     ) shouldBe TimeVector().days(4).hours( 8).parts(876)
      }
      it("leap year") {
        reminderForWeek(leapYear       ) shouldBe TimeVector().days(5).hours(21).parts(589)
      }
    }

    describe("Law 7") {
      it("molad Nisan example") {
        (TimeVector().hours(17).parts(107) + meanLunarPeriod).time shouldBe
          TimeVector().hours( 5).parts(900)
      }
    }

    describe("Law 8: First New Moons") {
      val year1newMoon = Year(1).newMoon

      it("year 1") {
        year1newMoon.day.name shouldBe Day.Name.Sheni
        // see also KH 6:13
        year1newMoon.time shouldBe TimeVector().hours(5).parts(204)
      }

      it("year 2") {
        val year2newMoon = Year(2).newMoon
        year2newMoon.day.name shouldBe Day.Name.Shishi
        year2newMoon.time shouldBe TimeVector().hours(14)

        (year2newMoon - meanLunarPeriod*12) shouldBe year1newMoon
      }
    }

    describe("Law 10") {
      it("year of Shmuel") {
        // see also 9:1-2
        LeapYearsCycle.yearsInCycle shouldBe 19
        LeapYearsCycle.leapYearsInCycle shouldBe 7
        Shmuel.yearLength shouldBe TimeVector().days(365).hours(6)
        LeapYearsCycle.cycleLength shouldBe (normalYear*12 + leapYear*7)
        (Shmuel.yearLength*LeapYearsCycle.yearsInCycle - LeapYearsCycle.cycleLength) shouldBe
          TimeVector().hours(1).parts(485)
        // KH 9:2
        Shmuel.seasonLength shouldBe TimeVector().days(91).hours(7).halfHour
      }
    }

    describe("Law 11") {
      it("leap years") {
        LeapYearsCycle.leapYears shouldBe Set(3, 6, 8, 11, 14, 17, 19)
      }
    }

    describe("Law 12") {
      it("cycle remainder") {
        reminderForWeek(TimeVector().days(4).hours( 8).parts(876)*12 +
          TimeVector().days(5).hours(21).parts(589)* 7) shouldBe
          TimeVector().days(2).hours(16).parts(595)
      }
    }
  }

  describe("Chapter 8") {
    describe("Laws 7-8") {
      it("year length for short years") {
        Jewish.Year.shortNonLeapYearLength shouldBe 353
        Jewish.Year.shortLeapYearLength shouldBe 383
      }
    }

    describe("Law 9") {
      it("kind of the year") {
        var numberRegular: Int = 0
        var numberFull: Int = 0
        var numberShort: Int = 0
        for (yearNumber <- 2 to 6000) {
          val year: Year = Year(yearNumber)
          if (!year.isLeap) {
            val nextYearFirstDay: Day = year.next.firstDay
            if (year.firstDay.is(Day.Name.Chamishi)) {
              if (nextYearFirstDay.is(Day.Name.Sheni)) {
                numberRegular += 1
                year.kind shouldBe Year.Kind.Regular
              }
              if (nextYearFirstDay.is(Day.Name.Shlishi)) {
                numberFull += 1
                year.kind shouldBe Year.Kind.Full
              }
            }
            if (year.firstDay.is(Day.Name.Shabbos)) {
              if (nextYearFirstDay.is(Day.Name.Shlishi)) {
                numberShort += 1
                year.kind shouldBe Year.Kind.Short
              }
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
        import Day.Name._
        for (yearNumber <- 1 to 6000) {
          val year = Year(yearNumber)
          val roshHashono = year.firstDay
          roshHashono.name match {
            case Shlishi  => assert(Year.Kind.Regular == year.kind)
            case Shabbos  => assert(Year.Kind.Regular != year.kind)
            case Sheni    => assert(Year.Kind.Regular != year.kind)
            case Chamishi =>
              if (year.isLeap) assert(Year.Kind.Regular != year.kind)
              else assert(Year.Kind.Short != year.kind)
            case _ => throw new IllegalArgumentException
          }
        }
      }
    }
  }

  describe("Chapter 9") {
    describe("Laws 3-4") {
      it("first tkufas Nisan for Shmuel") {
        Moon.firstMoladNisan shouldBe Year(1).month(Month.Name.Nisan).newMoon
        (Moon.firstMoladNisan - Shmuel.firstTkufasNisan) shouldBe
          TimeVector().days(7).hours(9).parts(642)
        Shmuel.firstTkufasNisan.day.name shouldBe Day.Name.Rvii
      }
    }

    describe("Laws 5-7") {
      it("tkufos of 4930") {
        val year: Year = Year(4930)

        LeapYearsCycle.forYear(year) shouldBe YearsCycle.In(260, 9)

        val tkufasNisan = Shmuel.seasonForYear(Season.TkufasNisan, year)
        tkufasNisan.day.name shouldBe Day.Name.Chamishi
        tkufasNisan.time shouldBe TimeVector().hours(6)
        tkufasNisan.day shouldBe year.month(Month.Name.Nisan).day(8)

        val tkufasTammuz = Shmuel.seasonForYear(Season.TkufasTammuz, year)
        tkufasTammuz.day.name shouldBe Day.Name.Chamishi
        tkufasTammuz.time shouldBe TimeVector().hours(13).halfHour

        val tkufasTishrei = Shmuel.seasonForYear(Season.TkufasTishrei, year)
        tkufasTishrei.day.name shouldBe Day.Name.Chamishi
        tkufasTishrei.time shouldBe TimeVector().hours(21)

        val tkufasTeves = Shmuel.seasonForYear(Season.TkufasTeves, year)
        tkufasTeves.day.name shouldBe Day.Name.Shishi
        tkufasTeves.time shouldBe TimeVector().hours(4).halfHour

        val nextTkufasNisan = Shmuel.seasonForYear(Season.TkufasNisan, year+1)
        nextTkufasNisan.day.name shouldBe Day.Name.Shishi
        nextTkufasNisan.time shouldBe TimeVector().hours(12)
      }
    }
  }

  describe("Chapter 10") {
    describe("Laws 1-2") {
      it("year of RavAda") {
        RavAda.yearLength shouldBe TimeVector().days(365).hours(5 ).parts(997).moments(48)
        (RavAda.yearLength - normalYear) shouldBe
          TimeVector().days( 10).hours(21).parts(121).moments(48)
        (RavAda.yearLength*LeapYearsCycle.yearsInCycle - LeapYearsCycle.cycleLength) shouldBe TimeVector.zero
        // KH 10:2
        RavAda.seasonLength shouldBe
          TimeVector().days(91).hours(7).parts(519).moments(31)
      }
    }

    describe("Law 3") {
      it("first tkufas Nisan for RavAda") {
        (Moon.firstMoladNisan - RavAda.firstTkufasNisan) shouldBe
          TimeVector().hours(9).parts(642)
        RavAda.firstTkufasNisan.day.name shouldBe Day.Name.Rvii
      }
    }
  }

  private def reminderForWeek(ofWhat: TimeVector): TimeVector =
    ofWhat - (week * (ofWhat.toRational / week.toRational).whole)
}
