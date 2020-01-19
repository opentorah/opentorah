package org.podval.calendar.astronomy

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.podval.calendar.angles.Angles
import Angles.{Position, Rotation, headRange, range}
import org.podval.calendar.jewish.{Jewish, LeapYearsCycle}
import Jewish.{Day, Month, Year}
import org.podval.calendar.dates.YearsCycle
import org.podval.calendar.numbers.BigRational

class TextTest extends AnyFunSpec with Matchers {
  describe("Chapter 11") {
    describe("Law 7") {
      it("angle units") {
        headRange shouldBe 360
        range(0) shouldBe 60
        range(1) shouldBe 60
        range(2) shouldBe 60
        range(3) shouldBe 60
        range(4) shouldBe 60
        range(5) shouldBe 60
        range(6) shouldBe 60
      }
    }

    describe("Laws 7-9") {
      it("zodiac") {
        Zodiac.values.length shouldBe 12
        Zodiac.Aries.start shouldBe Position(0)
        Zodiac.values.init.zip(Zodiac.values.tail).foreach {
          case (prev: Zodiac, next: Zodiac) =>
            (prev.start + Rotation(30)) shouldBe prev.end
            next.start shouldBe prev.end
        }

        Zodiac.Gemini  .at(Rotation(10, 30, 40)) shouldBe Position(70, 30, 40)
        Zodiac.Aquarius.at(Rotation(20        )) shouldBe Position(320)
      }
    }

    describe("Law 12") {
      it("angles subtraction") {
        (Position(100, 20, 30) - Rotation(200, 50, 40)) shouldBe Position(259, 29, 50)
      }
    }

    describe("Law 16") {
      it("epoch") {
        LeapYearsCycle.inCycle(260, 17) shouldBe 4938
        LeapYearsCycle.forNumber(4938) shouldBe YearsCycle.In(260, 17)
        Epoch.Text.day shouldBe Year(4938).month(Month.Name.Nisan).day(3)
        Epoch.Text.day.name shouldBe Day.Name.Chamishi
      }
    }
  }

  describe("Chapter 12") {
    describe("Law 2") {
      it("mean Sun") {
        val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Tammuz).day(14))
        result.day.name shouldBe Day.Name.Shabbos
        result.daysAfterEpoch shouldBe 100
        result.sunLongitudeMean shouldBe Position(105, 37, 25)
        result.sunLongitudeMean shouldBe Zodiac.Cancer.at(Rotation(15, 37, 25))
      }
    }
  }

  describe("Chapter 13") {
    describe("Laws 9-10") {
      it("true Sun") {
        val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Tammuz).day(14))
        result.day.name shouldBe Day.Name.Shabbos
        result.sunLongitudeMean shouldBe Position(105, 37, 25)
        result.sunApogee shouldBe Position(86, 45, 23)
        result.sunCourseRaw shouldBe Rotation(18, 52, 2)
        result.sunCourse shouldBe Rotation(19)
        result.sunLongitudeCorrection shouldBe -Rotation(0, 38)
        result.sunLongitudeTrueRaw shouldBe Position(104, 59, 25)
      }
    }
  }

  describe("Chapter 15") {
    describe("Laws 8-9") {
      it("true Moon") {
        val month: Month = Year(4938).month(Month.Name.Iyar)
        val day: Day = month.day(2)
        val result = Calculator.Text.calculate(day)
        result.day.name shouldBe Day.Name.Shishi
        result.daysAfterEpoch shouldBe 29
        result.sunLongitudeMean shouldBe Position(35, 38, 33)
        result.moonLongitudeMeanAtTimeOfSighting shouldBe Position(53, 36, 39)
        result.moonAnomalyMean shouldBe Position(103, 21, 46)
        result.elongation shouldBe Rotation(17, 58, 6)
        result.doubleElongation shouldBe Rotation(35, 56, 12)
        result.moonLongitudeDoubleElongationCorrection shouldBe Rotation(5)
        // result.moonAnomalyTrue shouldBe Position(108, 21)
        result.moonAnomalyTrue shouldBe Position(108)
        // KH 15:9
        result.moonAnomalyVisible shouldBe -Rotation(5, 1)
        result.moonLongitudeTrueRaw shouldBe Position(48, 35, 39)
        result.moonLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation(18, 36))
      }
    }
  }

  describe("Chapter 16") {
    describe("Laws 4-5") {
      it("Moon head") {
        val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
        result.day.name shouldBe Day.Name.Shishi
        result.daysAfterEpoch shouldBe 29
        // KH 16:5
        result.moonHeadMeanReversed shouldBe Position(182, 29, 37)
        result.moonHeadMeanRaw shouldBe Position(177, 30, 23)
        result.moonHeadMean shouldBe Zodiac.Virgo.at(Rotation(27, 30))
      }
    }

    describe("Law 12") {
      it("latitude interpolation") {
        Calculators.Text.moonLatitude(Rotation(53)) shouldBe Rotation(3, 59)
      }
    }

    describe("Laws 16-18") {
      it("latitude quadranting") {
        Calculators.Text.moonLatitude(Rotation(150)) shouldBe Rotation(2, 30)
        Calculators.Text.moonLatitude(Rotation(200)) shouldBe Rotation(1, 43)
        Calculators.Text.moonLatitude(Rotation(300)) shouldBe Rotation(4, 20)
      }
    }

    describe("Law 19") {
      it("Moon lattitude") {
        val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
        result.day.name shouldBe Day.Name.Shishi
        result.moonLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation(18, 36))
        result.moonHeadMean shouldBe Zodiac.Virgo.at(Rotation(27, 30))
        result.moonLatitudeCourseRaw.canonical shouldBe Rotation(231, 6)
        result.moonLatitude shouldBe Rotation(3, 53)
      }
    }
  }

  describe("Chapter 17") {
    describe("Laws 13-14") {
      it("arc of sighting") {
        val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
        result.day.name shouldBe Day.Name.Shishi
        result.sunLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation(7, 9))
        result.moonLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation(18, 36))
        result.moonLatitude shouldBe Rotation(3, 53)
        result.latitude1 shouldBe result.moonLatitude
        result.isMoonLatitudeNortherly shouldBe false
        result.longitude1 shouldBe Rotation(11, 27)
        result.longitudeSightingAdjustment shouldBe Rotation(1)
        result.longitude2 shouldBe Rotation(10, 27)
        result.latitudeSightingAdjustment shouldBe Rotation(0, 10)
        result.latitude2 shouldBe Rotation(4, 3)
        result.moonCircuitPortion shouldBe BigRational(1, 4)
        result.moonCircuit shouldBe Rotation(1, 1)
        // KH 17:14
        result.longitude3 shouldBe Rotation(11, 28)
        // Am I supposed to look at moonTrueLongitude?!
        result.moonLongitude3Portion shouldBe BigRational(1, 5)
        result.moonLongitude3Correction shouldBe Rotation(2, 18)
        result.longitude4 shouldBe Rotation(13, 46)
        result.geographicCorrection shouldBe Rotation(2, 35)
        result.arcOfSighting shouldBe Rotation(11, 11)
      }
    }

    describe("Law 22") {
      it("is sightable?") {
        val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
        result.day.name shouldBe Day.Name.Shishi
        result.arcOfSighting shouldBe Rotation(11, 11)
        result.longitude1 shouldBe Rotation(11, 27)
        result.isMoonSightable shouldBe true
      }
    }
  }
}
