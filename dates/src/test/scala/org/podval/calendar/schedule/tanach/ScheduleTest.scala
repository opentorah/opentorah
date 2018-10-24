package org.podval.calendar.schedule.tanach

import org.scalatest.{FlatSpec, Matchers}
import org.podval.calendar.jewish.{Jewish, YearType}
import YearType._
import Jewish.{Day, Year}
import org.podval.judaica.metadata.tanach.Parsha
import org.podval.judaica.metadata.tanach.Parsha._
import SpecialDay._

final class ScheduleTest extends FlatSpec with Matchers {

  "Torah readings" should "be assigned correctly" in {
    (2 to 6000) foreach { number =>
      val year = Year(number)

      verify(year, inHolyLand = false)
      verify(year, inHolyLand = true)
    }
  }

  def verify(year: Year, inHolyLand: Boolean): Unit = {
    val readings: Map[Day, WeeklyReading] = Schedule.weeklyReadingsForYear(year, inHolyLand)

    def findReadings(day: Day): WeeklyReading = readings(day)
    def isCombined(parsha: Parsha): Boolean = readings.exists(_._2.secondParsha.contains(parsha))

    // Pesach
    val readingsBeforePesach: WeeklyReading = findReadings(shabbosBefore(Pesach(year)))
    readingsBeforePesach.isCombined shouldBe false
    readingsBeforePesach.parsha shouldBe {
      if (!year.isLeap) Tzav else if (RoshHashanah1(year).is(Day.Name.Chamishi)) Acharei else Metzora
    }

    // Shavuot
    val readingsBeforeShavuot = findReadings(shabbosBefore(Shavuos(year)))
    readingsBeforeShavuot.isCombined shouldBe false
    Set[Parsha](Bemidbar, Nasso).contains(readingsBeforeShavuot.parsha) shouldBe true

    // Tisha Be Av
    findReadings(shabbosAfter(TishaBeAv(year))) shouldBe WeeklyReading(Va_eschanan, None)

    // Rosh Ha Shanah
    findReadings(shabbosBefore(RoshHashanah1(year+1))).parsha shouldBe Nitzavim
    val roshHaShanah: Day = RoshHashanah1(year+1)
    isCombined(Vayeilech) shouldBe !roshHaShanah.is(Day.Name.Sheni) && !roshHaShanah.is(Day.Name.Shlishi)

    val combined: Set[Parsha] = readings.values.toSet.filter(_.isCombined).map(_.parsha)
    val yearType = YearType.get(year)
    val combinedFromStructure: Seq[Parsha] = ReadingStructure.all(yearType).combined(inHolyLand)
    combined shouldBe combinedFromStructure.toSet
  }
}

object ReadingStructure {
  // This table gives parsha combinations for all occurring year types.
  // Submitted by @michaelko58; sourced from  https://www.shoresh.org.il/spages/articles/parashathibur.htm
  // Primary/classical source needs to be determined.
  sealed trait Combine {
    final def combined(parsha: Parsha, inHolyLand: Boolean): Option[Parsha] =
      if(combine(inHolyLand)) Some(parsha) else None

    def combine(inHolyLand: Boolean): Boolean
  }

  case object M extends Combine { // Mehubarim: combined
    def combine(inHolyLand: Boolean): Boolean = true
  }

  case object N extends Combine { // Nifradim: separate
    def combine(inHolyLand: Boolean): Boolean = false
  }

  case object C extends Combine { // mehubarim Chutz lo'oretz: combined in diaspora
    def combine(inHolyLand: Boolean): Boolean = !inHolyLand
  }

  final case class S
  (
    vp: Combine, // Vayakhel/Pekudei
    tm: Combine, // Tazria/Metzora
    ak: Combine, // Acharei/Kedoshim
    bb: Combine, // Behar/Bechukosai
    cb: Combine, // Chukas/Balak
    mm: Combine, // Mattos/Masai
    nv: Combine  // Nitzavim/Vayeilech
  ) {
    def combined(inHolyLand: Boolean): Seq[Parsha] = Seq[(Combine, Parsha)](
      vp -> Vayakhel,
      tm -> Tazria,
      ak -> Acharei,
      bb -> Behar,
      cb -> Chukas,
      mm -> Mattos,
      nv -> Nitzavim
    ).flatMap { case (c, p) => c.combined(p, inHolyLand) }
  }

  val all: Map[YearType, S] = Map(
    // non-leap
    N2S -> S(vp = M, tm = M, ak = M, bb = M, cb = N, mm = M, nv = M),
    N2F -> S(vp = M, tm = M, ak = M, bb = M, cb = C, mm = M, nv = M),
    N3R -> S(vp = M, tm = M, ak = M, bb = M, cb = C, mm = M, nv = M),
    N5R -> S(vp = M, tm = M, ak = M, bb = C, cb = N, mm = M, nv = N),
    N5F -> S(vp = N, tm = M, ak = M, bb = M, cb = N, mm = M, nv = N),
    N7S -> S(vp = M, tm = M, ak = M, bb = M, cb = N, mm = M, nv = N),
    N7F -> S(vp = M, tm = M, ak = M, bb = M, cb = N, mm = M, nv = M),

    // leap
    L2S -> S(vp = N, tm = N, ak = N, bb = N, cb = C, mm = M, nv = M),
    L2F -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = C, nv = N),
    L3R -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = C, nv = N),
    L4S -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = N, nv = N),
    L5F -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = N, nv = M),
    L7S -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = M, nv = M),
    L7F -> S(vp = N, tm = N, ak = N, bb = N, cb = C, mm = M, nv = M)
  )
}
