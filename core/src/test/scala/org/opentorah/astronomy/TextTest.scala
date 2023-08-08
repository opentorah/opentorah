package org.opentorah.astronomy

import org.opentorah.calendar.{Week, YearsCycle}
import org.opentorah.calendar.jewish.{Jewish, LeapYearsCycle, Sun}
import org.opentorah.numbers.BigRational
import Angles.{Digit, Position, Rotation, headRange, range}
import Jewish.{Day, Month, Year}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TextTest extends AnyFunSpec, Matchers:
  describe("Metadata") {
    it("Zodiacs should load correctly") {
      Zodiac.Aries.names.hasName("Овен") shouldBe true
    }
  }
  describe("Chapter 9") {
    describe("Law 1") {
      it("remainder of machzor - TODO") {
      }
      it("season length - TODO") {
      }
    }
    it("Law 3: first season of Nisan - TODO") {
    }
  }
  describe("Chapter 11") {
    it("Law 7: angle units") {
      headRange shouldBe 360
      range(0) shouldBe 60
      range(1) shouldBe 60
      range(2) shouldBe 60
      range(3) shouldBe 60
      range(4) shouldBe 60
      range(5) shouldBe 60
      range(6) shouldBe 60
    }

    it("Laws 7-9: zodiac") {
      Zodiac.valuesSeq.length shouldBe 12
      Zodiac.Aries.start shouldBe Position("0°")
      Zodiac.valuesSeq.init.zip(Zodiac.valuesSeq.tail).foreach((prev: Zodiac, next: Zodiac) =>
        (prev.start + Rotation("30°")) shouldBe prev.end
        next.start shouldBe prev.end
      )

      Zodiac.Gemini  .at(Rotation("10°30′40″")) shouldBe Position(" 70°30′40″")
      Zodiac.Aquarius.at(Rotation("20°      ")) shouldBe Position("320°      ")
    }

    it("Law 12: angles subtraction") {
      (Position("100°20′30″") - Rotation("200°50′40″")) shouldBe Position("259°29′50″")
    }

    it("Law 16: epoch") {
      LeapYearsCycle.inCycle(cycleNumber = 260, numberInCycle = 17) shouldBe 4938
      LeapYearsCycle.forNumber(4938) shouldBe YearsCycle.In(cycleNumber = 260, numberInCycle = 17)
      Epoch.Text.day shouldBe Year(4938).month(Month.Nisan).day(3)
      Epoch.Text.day.name shouldBe Week.Day.Chamishi
      Epoch.Text.day.number shouldBe 1803407
    }
  }

  describe("Chapter 12") {
    describe("Law 1: mean sun") {
      def verifyNested(data: Map[Days2Rotation.Days, Rotation.Interval]) =
        data(Days2Rotation.Days.One).contains(data(Days2Rotation.Days.Ten)) shouldBe true
        data(Days2Rotation.Days.Ten).contains(data(Days2Rotation.Days.Hundred)) shouldBe true
        data(Days2Rotation.Days.Hundred).contains(data(Days2Rotation.Days.Thousand)) shouldBe true
        data(Days2Rotation.Days.Thousand).contains(data(Days2Rotation.Days.TenThousand)) shouldBe true
      it(s"intervals other than month and year are nested (precision ${SunLongitudeMean.exactMinLength})") {
        verifyNested(SunLongitudeMean.exact(SunLongitudeMean.exactMinLength))
      }
      it("intervals other than month and year are nested (precision 6)") {
        verifyNested(SunLongitudeMean.exact(6))
      }
      it("v(1000) = 10*v(100)") {
        SunLongitudeMean.value(Days2Rotation.Days.Thousand) shouldBe (SunLongitudeMean.value(Days2Rotation.Days.Hundred)*10).canonical
      }
      it("v(10000) = 10*v(1000)") {
        SunLongitudeMean.value(Days2Rotation.Days.TenThousand) shouldBe (SunLongitudeMean.value(Days2Rotation.Days.Thousand)*10).canonical
      }
//      it should "calculate for 29 days in two steps" in :
//        (what.value(Days.Ten) * 3 - what.value(Days.One)) shouldBe what.value(Days.Month)
//        (what.value(Days.Ten) * 3 - what.value(Days.One)) shouldBe Rotation("28°35′1″")
//        (what.value(Days.Ten) * 2 + what.value(Days.One) * 9) shouldBe Rotation("28°34′58″")
      /*
       Al-Battani, WikiPedia:

       He also recalculated the values for the precession of the equinoxes (54.5" per year, or 1° in 66
       years) and the obliquity of the ecliptic (23° 35'), which was an elaboration of Hipparchus' work.
       */

      it("Almagest") {
        SunLongitudeMean.Opinion.Almagest.yearLengthRational shouldBe (BigRational(365) + BigRational(1, 4) - BigRational(1, 300))
        SunLongitudeMean.Opinion.Almagest.yearLength shouldBe Jewish.TimeVector("365ᵈ5ʰ993ᵖ46ᵐ")
        // TODO Point.toSecondLanguageString? 365 days, 5 hours, 55 minutes and 12 seconds
        SunLongitudeMean.Opinion.Almagest.movementInOneDay shouldBe Rotation("0°59′8″17‴13′‴12″‴31‴‴")
      }
      it("al-Battani") {
        SunLongitudeMean.Opinion.AlBattani.Pirush.FromMovement.movementInOneDay shouldBe Rotation("0°59′8″20‴35′‴")
        SunLongitudeMean.Opinion.AlBattani.Pirush.FromYearLength.yearLength shouldBe Jewish.TimeVector("365ᵈ5ʰ816ᵖ")
        SunLongitudeMean.Opinion.AlBattani.Pirush.FromYearLength.movementInOneDay shouldBe Rotation("0°59′8″21‴12′‴50″‴39‴‴")
        SunLongitudeMean.Opinion.AlBattani.Pirush.FromYearLength.movementInOneDay.roundTo(Angles.Digit.FOURTHS) shouldBe Rotation("0°59′8″21‴13′‴")
        SunLongitudeMean.Opinion.AlBattani.Pirush.FromYearLength.movementInOneDay.roundTo(Angles.Digit.FOURTHS) shouldNot
          be(SunLongitudeMean.Opinion.AlBattani.Pirush.FromMovement.movementInOneDay)
        (SunLongitudeMean.Opinion.AlBattani.Neugebauer.movementInOneDay*29) shouldBe Rotation("28°35′2″2‴41′‴10″‴46‴‴")
        (SunLongitudeMean.Opinion.AlBattani.Neugebauer.movementInOneDay*29).roundToSeconds shouldBe Rotation("28°35′2″")
      }
      it("Rav Ada") {
        SunLongitudeMean.Opinion.RavAda.yearLength shouldBe Jewish.Vector("365ᵈ5ʰ997ᵖ48ᵐ")
        // too small to reproduce 1,10,100,1000 and 10000
        SunLongitudeMean.Opinion.RavAda.movementInOneDay shouldBe Rotation("0°59′8″17‴7′‴46″‴8‴‴")
      }
      describe("Exact value") {
        it("round to the same as Almagest") {
          SunLongitudeMean.Opinion.Almagest.movementInOneDay.roundToSeconds shouldBe SunLongitudeMean.Opinion.Rambam.movementInOneDay.roundToSeconds
        }
        it("makes a full circle in a year") {
          assert(SunLongitudeMean.Opinion.Rambam.movementInOneDay * (Sun.RavAda.yearLength.toRational, Angles.maxLength) > Angles.period)
          assert(SunLongitudeMean.Opinion.Rambam.movementInOneDay * (Sun.Shmuel.yearLength.toRational, Angles.maxLength) > Angles.period)
        }
      }
      describe("Tzikuni") {
        it("exact value from v(10000)") {
          val tenThousand = SunLongitudeMean.value(Days2Rotation.Days.TenThousand)
          tenThousand shouldBe Rotation("136°28′20″")
          val fullYears: Int = 10000 / 365
          val exact1fourths = (tenThousand + Angles.period*fullYears)/(10000, 4)
          exact1fourths shouldBe Rotation("0°59′8″19‴48′‴")
          SunLongitudeMean.calculate(exact1fourths, Days2Rotation.Days.TenThousand) shouldBe tenThousand

          val exact1thirds = exact1fourths.roundTo(Digit.THIRDS)
          exact1thirds shouldBe Rotation("0°59′8″20‴")
          SunLongitudeMean.calculate(exact1thirds , Days2Rotation.Days.TenThousand) shouldBe Rotation("136°28′53″") // != tenThousand
        }
      }
      describe("sun at creation") {
        it("Nisan") {
          SunLongitudeMean.SunAtCreation.Nisan.velocity.from shouldBe Rotation("0°59′7″33‴58′‴27″‴")
          SunLongitudeMean.SunAtCreation.Nisan.velocity.to   shouldBe Rotation("0°59′7″33‴58′‴27″‴7‴‴")
        }
        it("Tishrei") {
          SunLongitudeMean.SunAtCreation.Tishrei.velocity.from shouldBe Rotation("0°59′8″16‴25′‴57″‴59‴‴")
          SunLongitudeMean.SunAtCreation.Tishrei.velocity.to   shouldBe Rotation("0°59′8″16‴25′‴58″‴5‴‴")
        }
      }
    }
    describe("Law 2") {
      describe("aphelion") {
        it("v(100) = 10*v(10)") {
          SunApogee.value(Days2Rotation.Days.Hundred) shouldBe (SunApogee.value(Days2Rotation.Days.Ten) * 10).canonical
        }
        it("v(1000) = 10*v(100)") {
          SunApogee.value(Days2Rotation.Days.Thousand) shouldBe (SunApogee.value(Days2Rotation.Days.Hundred) * 10).canonical
        }
        it("v(10000) = 10*v(1000)") {
          SunApogee.value(Days2Rotation.Days.TenThousand) shouldBe (SunApogee.value(Days2Rotation.Days.Thousand) * 10).canonical
        }
        it("intervals are nested (precision 6)") {
          val data = SunApogee.exact(6)
          data(Days2Rotation.Days.One).contains(data(Days2Rotation.Days.Ten)) shouldBe true
          data(Days2Rotation.Days.Ten).contains(data(Days2Rotation.Days.Thousand)) shouldBe true
          data(Days2Rotation.Days.Month).contains(data(Days2Rotation.Days.Thousand)) shouldBe true
          data(Days2Rotation.Days.Hundred).contains(data(Days2Rotation.Days.Thousand)) shouldBe true
          data(Days2Rotation.Days.Year).contains(data(Days2Rotation.Days.Thousand)) shouldBe true
          data(Days2Rotation.Days.Thousand).contains(data(Days2Rotation.Days.TenThousand)) shouldBe true
        }
        describe("Exact value") {
          it("is 0°0′0″9‴") {
            SunApogee.rambamValue shouldBe Rotation("0°0′0″9‴")
          }
          it("one degree in 66 years") {
            SunApogee.yearsForOneDegree.round shouldBe 66
          }
        }
      }
      it("mean sun example") {
        val result: Calculation = Calculator.Text.calculate(Year(4938).month(Month.Tammuz).day(14))
        result.day.name shouldBe Week.Day.Shabbos
        result.daysAfterEpoch shouldBe 100
        result.sunLongitudeMean shouldBe Position("105°37′25″")
        result.sunLongitudeMean shouldBe Zodiac.Cancer.at(Rotation("15°37′25″"))
      }
    }
  }

  describe("Chapter 13") {
    it("Laws 9-10: true Sun") {
      val result: Calculation = Calculator.Text.calculate(Year(4938).month(Month.Tammuz).day(14))
      result.day.name shouldBe Week.Day.Shabbos
      result.sunLongitudeMean shouldBe Position("105°37′25″")
      result.sunApogee shouldBe Position("86°45′23″")
      result.sunCourseRaw shouldBe Rotation("18°52′2″")
      result.sunCourse shouldBe Rotation("19°")
      result.sunLongitudeCorrection shouldBe -Rotation("0°38′")
      result.sunLongitudeTrueRaw shouldBe Position("104°59′25″")
    }
  }

  describe("Chapter 15") {
    it("Laws 8-9: true Moon") {
      val month: Month = Year(4938).month(Month.Iyar)
      val day: Day = month.day(2)
      val result = Calculator.Text.calculate(day)
      result.day.name shouldBe Week.Day.Shishi
      result.daysAfterEpoch shouldBe 29
      result.sunLongitudeMean shouldBe Position("35°38′33″")
      result.moonLongitudeMeanAtTimeOfSighting shouldBe Position("53°36′39″")
      result.moonAnomalyMean shouldBe Position("103°21′46″")
      result.elongation shouldBe Rotation("17°58′6″")
      result.doubleElongation shouldBe Rotation("35°56′12″")
      result.moonLongitudeDoubleElongationCorrection shouldBe Rotation("5°")
      // result.moonAnomalyTrue shouldBe Position("108°21′")
      result.moonAnomalyTrue shouldBe Position("108°")
      // KH 15:9
      result.moonAnomalyVisible shouldBe -Rotation("5°1′")
      result.moonLongitudeTrueRaw shouldBe Position("48°35′39″")
      result.moonLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation("18°36′"))
    }
  }

  describe("Chapter 16") {
    it("Laws 4-5: Moon head") {
      val result = Calculator.Text.calculate(Year(4938).month(Month.Iyar).day(2))
      result.day.name shouldBe Week.Day.Shishi
      result.daysAfterEpoch shouldBe 29
      // KH 16:5
      result.moonHeadMeanReversed shouldBe Position("182°29′37″")
      result.moonHeadMeanRaw shouldBe Position("177°30′23″")
      result.moonHeadMean shouldBe Zodiac.Virgo.at(Rotation("27°30′"))
    }

    it("Law 12: latitude interpolation") {
      Calculators.Text.moonLatitude(Rotation("53°")) shouldBe Rotation("3°59′")
    }

    it("Laws 16-18: latitude quadranting") {
      Calculators.Text.moonLatitude(Rotation("150°")) shouldBe Rotation("2°30′")
      Calculators.Text.moonLatitude(Rotation("200°")) shouldBe Rotation("1°43′")
      Calculators.Text.moonLatitude(Rotation("300°")) shouldBe Rotation("4°20′")
    }

    it("Law 19: Moon lattitude") {
      val result = Calculator.Text.calculate(Year(4938).month(Month.Iyar).day(2))
      result.day.name shouldBe Week.Day.Shishi
      result.moonLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation("18°36′"))
      result.moonHeadMean shouldBe Zodiac.Virgo.at(Rotation("27°30′"))
      result.moonLatitudeCourseRaw.canonical shouldBe Rotation("231°6′")
      result.moonLatitude shouldBe Rotation("3°53′")
    }
  }

  describe("Chapter 17") {
    it("Laws 13-14: arc of sighting") {
      val result = Calculator.Text.calculate(Year(4938).month(Month.Iyar).day(2))
      result.day.name shouldBe Week.Day.Shishi
      result.sunLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation("7°9′"))
      result.moonLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation("18°36′"))
      result.moonLatitude shouldBe Rotation("3°53′")
      result.latitude1 shouldBe result.moonLatitude
      result.isMoonLatitudeNortherly shouldBe false
      result.longitude1 shouldBe Rotation("11°27′")
      result.longitudeSightingAdjustment shouldBe Rotation("1°")
      result.longitude2 shouldBe Rotation("10°27′")
      result.latitudeSightingAdjustment shouldBe Rotation("0°10′")
      result.latitude2 shouldBe Rotation("4°3′")
      result.moonCircuitPortion shouldBe BigRational(1, 4)
      result.moonCircuit shouldBe Rotation("1°1′")
      // KH 17:14
      result.longitude3 shouldBe Rotation("11°28′")
      // Am I supposed to look at moonTrueLongitude?!
      result.moonLongitude3Portion shouldBe BigRational(1, 5)
      result.moonLongitude3Correction shouldBe Rotation("2°18′")
      result.longitude4 shouldBe Rotation("13°46′")
      result.geographicCorrection shouldBe Rotation("2°35′")
      result.arcOfSighting shouldBe Rotation("11°11′")
    }

    it("Law 22: is sightable?") {
      val result = Calculator.Text.calculate(Year(4938).month(Month.Iyar).day(2))
      result.day.name shouldBe Week.Day.Shishi
      result.arcOfSighting shouldBe Rotation("11°11′")
      result.longitude1 shouldBe Rotation("11°27′")
      result.isMoonSightable shouldBe true
    }
  }
