package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.{Attributes, Metadata, Named, Names, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Aliyot, Custom, Span}
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.Jewish.Month.Name._
import org.podval.calendar.jewish.{JewishDay, JewishYear}

import scala.xml.Elem

// TODO synthesize names in known languages
// TODO do not require Private to extends Named
sealed abstract class SpecialDay(override val name: String) extends Named {
  final override def names: Names = metadata.names

  protected def metadata: SpecialDay.SpecialDayMetadata = SpecialDay.metadatas(this)
}

object SpecialDay {

  sealed trait WithDate {
    def apply(year: Year): Day

    def corrected(year: Year): Day = apply(year)
  }

  sealed trait DayOf extends WithDate {
    def firstDay: WithDate

    def dayNumber: Int
  }

  sealed trait FirstDayOf extends DayOf {
    final override def firstDay: WithDate = this

    final override def dayNumber: Int = 1
  }

  sealed trait NonFirstDayOf extends DayOf {
    final override def apply(year: Year): Day = firstDay(year) + (dayNumber-1)
  }

  sealed trait WithReading {
    def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading

    def getAfternoonReading: Option[Reading] = None
  }

  sealed trait Loaded { this: SpecialDay =>
    final def getAliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] = {
      val result = metadata
      (if (isShabbos) result.shabbosAliyot else result.weekdayAliyot).get.getAliyot
    }

    final def weekdayAliyot: Seq[ChumashSpan.BookSpan] = getAliyot(isShabbos = false)

    def maftir: Option[ChumashSpan.BookSpan] = Some(metadata.maftir.get)

    def haftarah: Option[Haftarah] = Some(metadata.haftarah.get)
  }

  sealed trait WithSimpleReading extends WithReading with Loaded { self: SpecialDay =>
    final override def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading = {
      if (isShabbos) require(isShabbosAllowed)
      require(weeklyReading.isEmpty)
      getReading(isShabbos)
    }

    protected def isShabbosAllowed: Boolean = true

    def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = aliyot(isShabbos),
      maftir = maftir,
      haftarah = haftarah
    )

    def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] =
      aliyotSameAs(isShabbos).getAliyot(isShabbos)

    protected def aliyotSameAs(isShabbos: Boolean): WithSimpleReading = this
  }

  sealed trait NoShabbosAllowed { this: WithSimpleReading =>
    protected final override def isShabbosAllowed: Boolean = false
  }

  sealed class Private(name: String) extends SpecialDay(name) with Loaded

  sealed trait FestivalOrIntermediate extends SpecialDay with WithDate with WithReading

  sealed trait Festival extends FestivalOrIntermediate with WithSimpleReading

  sealed trait SecondDay extends Festival with NonFirstDayOf {
    def secondDayOf: Festival

    override def maftir: Option[ChumashSpan.BookSpan] = secondDayOf.maftir
  }

  sealed abstract class Intermediate(name: String, intermediateDayNumber: Int, inHolyLand: Boolean)
    extends SpecialDay(name) with FestivalOrIntermediate with NonFirstDayOf
  {
    override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)
  }

  sealed trait RabbinicFestival extends WithReading with WithDate

  sealed trait Fast extends WithSimpleReading with WithDate with NoShabbosAllowed { this: SpecialDay => }

  case object ShabbosErevRoshChodesh extends SpecialDay("Shabbos Erev Rosh Chodesh")
  case object RoshChodesh extends SpecialDay("Rosh Chodesh")
  private case object RoshChodeshPart2 extends SpecialDay("Rosh Chodesh Part 2")
  case object ShabbosRoshChodeshAdditionalHaftorah extends SpecialDay("Shabbos Rosh Chodesh Additional Haftorah")
  case object ShabbosErevRoshChodeshAdditionalHaftorah extends SpecialDay("Shabbos Erev Rosh Chodesh Additional Haftorah")

  sealed abstract class FastLike(name: String) extends SpecialDay(name) with Fast with NoShabbosAllowed {
    final override def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] =
      FastReading.getAliyot(isShabbos) ++ FastPart2.getAliyot(isShabbos)

    final override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = aliyot(isShabbos),
      maftir = None,
      haftarah = None
    )

    final override def getAfternoonReading: Option[Reading] = {
      val all = aliyot(false)
      Some(Reading(
        aliyot = all.init,
        maftir = Some(all.last),
        haftarah = Some(afternoonHaftarah.get)
      ))
    }

    protected def afternoonHaftarah: Option[Haftarah] = FastPart2.haftarah
  }

  private case object FastReading extends Private("Fast")

  private case object FastPart2 extends Private("Fast Part 2")

  private case object IntermediateShabbos extends Private("Intermediate Shabbos")

  case object RoshHashanah1 extends SpecialDay("Rosh Hashanah 1") with Festival with FirstDayOf {
    override def apply(year: JewishYear): JewishDay = year.month(Tishrei).day(1)
  }

  case object RoshHashanah2 extends SpecialDay("Rosh Hashanah 2")
    with SecondDay with NonFirstDayOf with NoShabbosAllowed
  {
    override def secondDayOf: Festival = RoshHashanah1

    override def firstDay: WithDate = RoshHashanah1

    override def dayNumber: Int = 2
  }

  case object FastOfGedalia extends FastLike("Fast of Gedalia") {
    final override def apply(year: Year): Day = year.month(Tishrei).day(3)

    override def corrected(year: Year): Day = {
      val result = apply(year)
      if (result.isShabbos) result+1 else result
    }

    protected override def afternoonHaftarah: Option[Haftarah] = FastOfGedalia.haftarah
  }

  case object YomKippur extends SpecialDay("Yom Kippur") with Festival {
    override def apply(year: JewishYear): JewishDay = year.month(Tishrei).day(10)

    override def getAfternoonReading: Option[Reading] = Some(Reading(
      aliyot = YomKippurAfternoon.getAliyot(false).init,
      maftir = Some(YomKippurAfternoon.getAliyot(false).last),
      haftarah = Some(YomKippurAfternoon.haftarah.get)
    ))
  }

  private case object YomKippurAfternoon extends Private("Yom Kippur Afternoon")

  case object Succos extends SpecialDay("Succos") with Festival with FirstDayOf {
    override def apply(year: JewishYear): JewishDay = year.month(Tishrei).day(15)
  }

  case object Succos2 extends SpecialDay("Succos 2") with SecondDay {
    override def firstDay: WithDate = Succos

    override def dayNumber: Int = 2

    override def secondDayOf: Festival = Succos

    protected override def aliyotSameAs(isShabbos: Boolean): WithSimpleReading = Succos
  }

  // TODO mapping to the readings skips Shabbos - whatever that means... Or was it meant for Pesach only?
  sealed class SuccosIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate("Succos Intermediate " + intermediateDayNumber, intermediateDayNumber, inHolyLand)
      with WithSimpleReading
  {
    override def firstDay: WithDate = Succos

    override def getReading(isShabbos: Boolean): Reading ={
      val all: Seq[ChumashSpan.BookSpan] = SuccosIntermediateReading.weekdayAliyot
      def korbanot(n: Int): ChumashSpan.BookSpan = all(n-1)

      if (intermediateDayNumber == 6) require(inHolyLand)
      val last: ChumashSpan.BookSpan =
        if (inHolyLand) korbanot(intermediateDayNumber)
        else korbanot(intermediateDayNumber) + korbanot(intermediateDayNumber+1)

      if (isShabbos) Reading(
        aliyot = IntermediateShabbos.getAliyot(isShabbos),
        maftir = Some(last),
        haftarah = SuccosIntermediateReading.haftarah // TODO ?
      ) else {
        // TODO Custom RavNae: on nth day of Sukkos, all alios (1 - 4) = korbanot(n)
        val n: Int = if (intermediateDayNumber <= 4) intermediateDayNumber else 4
        val first3: Seq[ChumashSpan.BookSpan] = Seq(korbanot(n), korbanot(n+1), korbanot(n+2))
        Reading(first3 :+ last, None, None)
      }
    }
  }

  private case object SuccosIntermediateReading extends Private("Succos Intermediate")

  case object SuccosIntermediate1 extends SuccosIntermediate(1, false)
  case object SuccosIntermediate2 extends SuccosIntermediate(2, false)
  case object SuccosIntermediate3 extends SuccosIntermediate(3, false)
  case object SuccosIntermediate4 extends SuccosIntermediate(4, false)
  case object HoshanahRabbah      extends SuccosIntermediate(5, false)

  case object SuccosIntermediate1InHolyLand extends SuccosIntermediate(1, true)
  case object SuccosIntermediate2InHolyLand extends SuccosIntermediate(2, true)
  case object SuccosIntermediate3InHolyLand extends SuccosIntermediate(3, true)
  case object SuccosIntermediate4InHolyLand extends SuccosIntermediate(4, true)
  case object SuccosIntermediate5InHolyLand extends SuccosIntermediate(5, true)
  case object HoshanahRabbahInHolyLand      extends SuccosIntermediate(6, true)

  case object SheminiAtzeres extends SpecialDay("Shemini Atzeres") with Festival with NonFirstDayOf {
    override def firstDay: WithDate = Succos
    override def dayNumber: Int = 8
  }

  case object SimchasTorah extends SpecialDay("Simchas Torah") with SecondDay with NoShabbosAllowed {
    override def firstDay: WithDate = Succos

    override def dayNumber: Int = 9

    override def secondDayOf: Festival = SheminiAtzeres

    override def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] =
      getAliyot(isShabbos) ++ SimchasTorahChassanBereishis.getAliyot(isShabbos)
  }

  private case object SimchasTorahChassanBereishis extends Private("Simchas Torah Chassan Bereishis")

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends SpecialDay("Shemini Atzeres and Simchas Torah")
    with Festival with NonFirstDayOf
  {
    override def firstDay: WithDate = Succos

    override def dayNumber: Int = 8

    override def getReading(isShabbos: Boolean): Reading = SimchasTorah.getReading(isShabbos)

    override def getAfternoonReading: Option[Reading] = SimchasTorah.getAfternoonReading
  }

  case object ShabbosBereishis extends SpecialDay("Shabbos Bereishis") with WithDate {
    override def apply(year: Year): Day = shabbosAfter(SimchasTorah(year))
  }

  sealed class Chanukah(override val dayNumber: Int) extends SpecialDay("Chanukah " + dayNumber)
    with DayOf with RabbinicFestival
  {
    final override def firstDay: WithDate = Chanukah1

    override def apply(year: Year): Day = Chanukah1(year)+(dayNumber-1)

    final override def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading = {
      val all: Seq[ChumashSpan.BookSpan] = ChannukahReading.weekdayAliyot
      def first(n: Int): ChumashSpan.BookSpan = all(2*n-1)
      def second(n: Int): ChumashSpan.BookSpan = all(2*n  )
      def split(n: Int): Seq[ChumashSpan.BookSpan] = Seq(first(n), second(n))
      def full(n: Int): ChumashSpan.BookSpan = ChumashSpan.merge(split(n))

      if (isRoshChodesh) require((dayNumber == 6) || (dayNumber == 7))
      if (dayNumber == 6) require(isRoshChodesh)

      if (isShabbos) Reading(
        aliyot = weeklyReading.get.getReading.aliyot,
        maftir = Some(full(dayNumber)),
        haftarah = (if (dayNumber < 8) ChannukahShabbos1 else ChannukahShabbos2).haftarah
      )
      //      else  if (isRoshChodesh) {
      //      RULE: when a day of Channukah falls on Rosh Chodesh (6th - always; 7th - sometimes),
      //      first 3 aliyot are from Rosh Chodesh (see above); aliyah 4: n.1+n.2
      //          ???
      //        }
      else {
        val result: Custom.Of[Seq[ChumashSpan.BookSpan]] =
          if (dayNumber == 1) Map(
            Custom.Ashkenaz -> (all.head +: split(1)),
            Custom.Sefard -> Seq(all.head, full(1), full(2))
          ) else if (dayNumber < 8) {
            val common = split(dayNumber)
            val ashkenazAndChabad = common :+ full(if (dayNumber < 7) dayNumber+1 else 1)
            Map(
              Custom.Ashkenaz -> ashkenazAndChabad,
              Custom.Chabad -> ashkenazAndChabad,
              Custom.Sefard -> (common :+ full(dayNumber))
            )
          } else {
            val endAliyot = ChannukahEnd.weekdayAliyot
            require(endAliyot.length == 1)
            val end = endAliyot.head
            val common = split(1)
            Map(
              Custom.Ashkenaz -> (common :+ end),
              Custom.Sefard -> (common :+ ChumashSpan.merge(split(1) :+ end))
            )
          }

        Reading(
          aliyot = result,
          maftir = None,
          haftarah = None
        )
      }
    }
  }
  private case object ChannukahReading extends Private("Channukah")
  private case object ChannukahEnd extends Private("Channukah End")
  private case object ChannukahShabbos1 extends Private("Channukah Shabbos 1")
  private case object ChannukahShabbos2 extends Private("Channukah Shabbos 2")

  case object Chanukah1 extends Chanukah(1) {
    override def apply(year: Year): Day = year.month(Kislev).day(25)
  }
  case object Chanukah2 extends Chanukah(2)
  case object Chanukah3 extends Chanukah(3)
  case object Chanukah4 extends Chanukah(4)
  case object Chanukah5 extends Chanukah(5)
  case object Chanukah6 extends Chanukah(6)
  case object Chanukah7 extends Chanukah(7)
  case object Chanukah8 extends Chanukah(8)

  case object FastOfTeves extends FastLike("Fast of 10th of Teves") {
    final def apply(year: Year): Day = year.month(Teves).day(10)
  }

  // TODO FastLike
  case object FastOfEster extends SpecialDay("fast of Ester") with Fast {
    override def apply(year: Year): Day = Purim(year)-1

    override def corrected(year: Year): Day = {
      val result = apply(year)
      // If on Friday or Saturday - move to Thursday
      if (result.isShabbos) result-2 else
      if (result.next.isShabbos) result-1 else
        result
    }
  }

  // TODO add in Schedule:

  case object ParshasShekalim extends SpecialDay("Parshas Shekalim") with WithDate {
    override def apply(year: Year): Day = {
      val result = Purim(year).month.firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }
  }

  case object ParshasZachor extends SpecialDay("Parshas Zachor") with WithDate {
    override def apply(year: Year): Day = shabbosBefore(Purim(year))
  }

  case object ParshasParah extends SpecialDay("Parshas Parah") with WithDate {
    override def apply(year: Year): Day = shabbosBefore(ParshasHachodesh(year))
  }

  case object ParshasHachodesh extends SpecialDay("Parshas Hachodesh") with WithDate {
    override def apply(year: Year): Day = {
      val result = year.month(Nisan).firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }
  }

  // TODO in Schedule
  case object ShabbosHagodol extends SpecialDay("Shabbos Hagodol") with WithDate {
    override def apply(year: Year): Day = shabbosBefore(Pesach(year))
  }

  case object Purim extends SpecialDay("Purim") with RabbinicFestival with WithSimpleReading with NoShabbosAllowed {
    override def apply(year: Year): Day = year.latestAdar.day(14)

    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = aliyot(isShabbos),
      maftir = None,
      haftarah = None
    )
  }
  case object ShushanPurim extends SpecialDay("Shushan Purim") with WithDate {
    override def apply(year: Year): Day = Purim(year) + 1
  }

  case object Pesach extends SpecialDay("Pesach")
    with FirstDayOf with WithSimpleReading with Festival
  {
    final def apply(year: Year): Day = year.month(Nisan).day(15)
  }

  case object Pesach2 extends SpecialDay("Pesach 2")
    with NonFirstDayOf with SecondDay with WithSimpleReading with NoShabbosAllowed
  {
    override def firstDay: WithDate = Pesach

    override def dayNumber: Int = 2

    override def secondDayOf: Festival = Pesach
  }

  sealed class PesachIntermediate(
    intermediateDayNumber: Int,
    inHolyLand: Boolean
  ) extends Intermediate("Pesach Intermediate " + intermediateDayNumber, intermediateDayNumber, inHolyLand) {
    override def firstDay: WithDate = Pesach

    final override def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading = {
      if (isShabbos) {
        require((dayNumber != 2) && (dayNumber != 6))
        Reading(
          aliyot = IntermediateShabbos.getAliyot(isShabbos),
          maftir = Pesach7.maftir,
          haftarah = PesachIntermediateShabbos.haftarah
        )
      } else {
        val reading: Loaded = dayNumber match {
          case 2 => Pesach2InHolyLand
          case 3 => Pesach3
          case 4 => if (!isPesachOnChamishi) Pesach4 else Pesach3
          case 5 => if (isShabbos || !isPesachOnChamishi) Pesach5 else Pesach4
          case 6 => Pesach6
        }
        Reading(
          aliyot = reading.getAliyot(isShabbos),
          maftir = reading.maftir,
          haftarah = reading.haftarah
        )
      }
    }

    // TODO!!!
//    override def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] =
//      super.aliyot(isShabbos) :+ Pesach7.maftir.get
  }

  case object PesachIntermediate1 extends PesachIntermediate(1, false)
  case object PesachIntermediate2 extends PesachIntermediate(2, false)
  case object PesachIntermediate3 extends PesachIntermediate(3, false)
  case object PesachIntermediate4 extends PesachIntermediate(4, false)

  case object PesachIntermediate1InHolyLand extends PesachIntermediate(1, true)
  case object PesachIntermediate2InHolyLand extends PesachIntermediate(2, true)
  case object PesachIntermediate3InHolyLand extends PesachIntermediate(3, true)
  case object PesachIntermediate4InHolyLand extends PesachIntermediate(4, true)
  case object PesachIntermediate5InHolyLand extends PesachIntermediate(5, true)


  private case object PesachIntermediateShabbos extends Private("Pesach Intermediate Shabbos")
  private case object Pesach2InHolyLand extends Private("Pesach 2 in Holy Land")
  private case object Pesach3 extends Private("Pesach 3")
  private case object Pesach4 extends Private("Pesach 4")
  private case object Pesach5 extends Private("Pesach 5")
  private case object Pesach6 extends Private("Pesach 6")


  case object Pesach7 extends SpecialDay("Pesach 7") with Festival with NonFirstDayOf with WithSimpleReading {
    override def firstDay: WithDate = Pesach

    override def dayNumber: Int = 7
  }

  case object Pesach8 extends SpecialDay("Pesach 8") with NonFirstDayOf with SecondDay {
    override def firstDay: WithDate = Pesach

    override def dayNumber: Int = 8

    override def secondDayOf: Festival = Pesach7

    override def aliyotSameAs(isShabbos: Boolean): WithSimpleReading =
      if (!isShabbos) this else SheminiAtzeres
  }

  case object LagBaOmer extends SpecialDay("Lag Ba Omer") with WithDate {
    override def apply(year: Year): Day = year.month(Iyar).day(18)
  }

  case object Shavuos extends SpecialDay("Shavuos") with FirstDayOf with Festival with NoShabbosAllowed {
    override def apply(year: Year): Day = year.month(Sivan).day(6)
  }

  case object Shavuos2 extends SpecialDay("Shavuos 2") with NonFirstDayOf with SecondDay {
    override def firstDay: WithDate = Shavuos

    override def dayNumber: Int = 2

    override def secondDayOf: Festival = Shavuos

    override def aliyotSameAs(isShabbos: Boolean): WithSimpleReading =
      if (!isShabbos) Pesach8 else SheminiAtzeres
  }

  case object FastOfTammuz extends FastLike("Fast of Tammuz") {
    override def apply(year: Year): Day = year.month(Tammuz).day(17)

    override def corrected(year: Year): Day = {
      val result = apply(year)
      if (result.isShabbos) result+1 else result
    }
  }

  case object TishaBeAv extends SpecialDay("Tisha BeAv") with Fast with NoShabbosAllowed {
    override def apply(year: Year): Day = year.month(Av).day(9)

    override def corrected(year: Year): Day = {
      val result = apply(year)
      if (result.isShabbos) result+1 else result
    }

    override def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] = getAliyot(false).init
    override def maftir: Option[ChumashSpan.BookSpan] = Some(getAliyot(false).last)

    override def getAfternoonReading: Option[Reading] = Some(Reading(
      aliyot = aliyot(false),
      maftir = maftir,
      haftarah = TishaBeAvAfternoon.haftarah
    ))
  }

  private case object TishaBeAvAfternoon extends Private("Tisha BeAv Afternoon")

  val festivals: Set[FestivalOrIntermediate] = Set(
    RoshHashanah1, RoshHashanah2,
    YomKippur,
    Succos, Succos2,
    SuccosIntermediate1, SuccosIntermediate2, SuccosIntermediate3, SuccosIntermediate4,
    HoshanahRabbah, SheminiAtzeres, SimchasTorah,
    Pesach, Pesach2, PesachIntermediate1, PesachIntermediate2, PesachIntermediate3, PesachIntermediate4, Pesach7, Pesach8,
    Shavuos, Shavuos2
  )

  val festivalsInHolyLand: Set[FestivalOrIntermediate] = Set(
    RoshHashanah1, RoshHashanah2,
    YomKippur,
    Succos,
    SuccosIntermediate1InHolyLand, SuccosIntermediate2InHolyLand, SuccosIntermediate3InHolyLand,
    SuccosIntermediate4InHolyLand, SuccosIntermediate5InHolyLand,
    HoshanahRabbahInHolyLand, SheminiAtzeresAndSimchasTorahInHolyLand,
    Pesach, PesachIntermediate1InHolyLand, PesachIntermediate2InHolyLand, PesachIntermediate3InHolyLand,
    PesachIntermediate4InHolyLand, PesachIntermediate5InHolyLand, Pesach7,
    Shavuos
  )

  def festivals(inHolyLand: Boolean): Set[FestivalOrIntermediate] = if (inHolyLand) festivalsInHolyLand else festivals

  val rabbinicFestivals: Set[RabbinicFestival] = Set(
    Chanukah1, Chanukah2, Chanukah3, Chanukah4, Chanukah5, Chanukah6, Chanukah7, Chanukah8,
    Purim // ShushanPurim
  )

  val fasts: Set[Fast] = Set(FastOfGedalia, FastOfTeves, FastOfEster, FastOfTammuz, TishaBeAv)

  val daysWithSpecialReadingsNotFestivals: Set[WithDate with WithReading] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[WithReading] =
    Set.empty[WithReading] ++ festivals(inHolyLand) ++ fasts ++ rabbinicFestivals

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)

  // Needs to be lazy for initialization/metadata loading to work...
  private lazy val values: Seq[SpecialDay] = Seq(
    ShabbosErevRoshChodesh, ShabbosErevRoshChodeshAdditionalHaftorah,
    RoshChodesh, RoshChodeshPart2, ShabbosRoshChodeshAdditionalHaftorah,
    FastReading, FastPart2, FastOfGedalia, TishaBeAv, TishaBeAvAfternoon,
    IntermediateShabbos,
    RoshHashanah1, RoshHashanah2, YomKippur, YomKippurAfternoon,
    Succos, Succos2, SuccosIntermediateReading,
    SheminiAtzeres, SimchasTorah, SimchasTorahChassanBereishis,
    ChannukahReading, ChannukahEnd, ChannukahShabbos1, ChannukahShabbos2,
    ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh,
    Purim, ShabbosHagodol,
    PesachIntermediateShabbos,
    Pesach, Pesach2InHolyLand, Pesach2, Pesach3,
    Pesach4, Pesach5, Pesach6, Pesach7, Pesach8,
    Shavuos, Shavuos2
  )

  final case class SpecialDayMetadata(
    names: Names,
    weekdayAliyot: Option[Aliyot],
    shabbosAliyot: Option[Aliyot],
    maftir: Option[ChumashSpan.BookSpan],
    haftarah: Option[Haftarah]
  )

  private lazy val metadatas: Map[SpecialDay, SpecialDayMetadata] = Metadata.loadMetadata(
    keys = values,
    obj = SpecialDay.this,
    elementName = "day"
  ).map { case (day, metadata) =>
    metadata.attributes.close()

    val (torahElements, maftriElements, haftarahElements) = XML.span(metadata.elements,
      "torah", "maftir", "haftarah")

    val (weekdayAliyot: Option[Aliyot], shabbosAliyot: Option[Aliyot]) =
      XML.noMoreThanOne(torahElements).fold[(Option[Aliyot], Option[Aliyot])]((None, None))(parseTorah)

    day -> SpecialDayMetadata(
      metadata.names,
      weekdayAliyot = weekdayAliyot,
      shabbosAliyot = shabbosAliyot,
      maftir = XML.noMoreThanOne(maftriElements).map(parseMaftir),
      haftarah = XML.noMoreThanOne(haftarahElements).map(parseHaftarah)
    )
  }

  private def parseTorah(element: Elem): (Option[Aliyot], Option[Aliyot]) = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: ChumashSpan.BookSpan = ChumashSpan.parse(attributes).resolve
    attributes.close()

    def parseAliyot(elements: Seq[Elem]): Aliyot = {
      val fromChapter: Int = bookSpan.span.from.chapter
      val result = elements.map(element => XML.parseEmpty(element, "aliyah", parseNumbered(fromChapter)))
      Aliyot(bookSpan, result, number = None)
    }

    val (weekdayElements, shabbos) = XML.span(elements, "aliyah", "shabbos")
    val shabbosElements: Option[Seq[Elem]] = XML.noMoreThanOne(shabbos).map { shabbos =>
      val (shabbosAttributes, shabbosElements) = XML.open(shabbos, "shabbos")
      shabbosAttributes.close()
      XML.span(shabbosElements, "aliyah")
    }

    val weekdayAliyot =
      if (weekdayElements.nonEmpty) Some(parseAliyot(weekdayElements))
      else if (shabbosElements.isEmpty) Some(Aliyot(bookSpan, Seq(bookSpan.span)))
      else None

    val shabbosAliyot = shabbosElements.map(elements => parseAliyot(elements))

    shabbosAliyot.foreach(aliyot => require(aliyot.aliyot.length == 7))

    (weekdayAliyot, shabbosAliyot)
  }

  private def parseNumbered(fromChapter: Int)(attributes: Attributes): Span.Numbered = {
    val span: Span.SemiResolved = Span.parse(attributes).defaultFromChapter(fromChapter).semiResolve
    require(span.to.isEmpty, s"Non-empty to: $span")
    Span.Numbered(
      n = attributes.doGetInt("n"),
      span = span
    )
  }

  private def parseMaftir(element: Elem): ChumashSpan.BookSpan =
    XML.parseEmpty(element, "maftir", ChumashSpan.parse).resolve

  private def parseHaftarah(element: Elem): Haftarah = {
    val (attributes, elements) = XML.open(element, "haftarah")
    Haftarah(attributes, elements)
  }
}
