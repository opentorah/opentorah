package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.{Attributes, Metadata, Named, Names, WithName, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Aliyot, Custom, Span}
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.Jewish.Month.Name._
import org.podval.calendar.jewish.{JewishDay, JewishYear}

import scala.xml.Elem

// TODO only non-synthesized WithReading ones - named!
// WithMetadata; WithReading doesn't imply Loaded...
// TODO synthesize names in known languages

object SpecialDay {

  sealed trait WithMetadata extends WithName {
    protected def metadata: SpecialDayMetadata = metadatas(this)
  }

  sealed trait LoadedNames extends Named with WithMetadata {
    final override def names: Names = metadata.names
  }

  sealed trait Date {
    def date(year: Year): Day

    final def correctedDate(year: Year): Day = correctDate(date(year))

    protected def correctDate(date: Day): Day = date
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

  sealed trait DayOf extends Date {
    def firstDay: Date

    def dayNumber: Int
  }

  sealed trait FirstDayOf extends DayOf {
    final override def firstDay: Date = this

    final override def dayNumber: Int = 1
  }

  sealed trait NonFirstDayOf extends DayOf {
    final override def date(year: Year): Day = firstDay.date(year) + (dayNumber-1)
  }

  sealed trait Loaded extends WithMetadata {
    final def getAliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] = {
      val result = metadata
      (if (isShabbos) result.shabbosAliyot else result.weekdayAliyot).get.getAliyot
    }

    final def weekdayAliyot: Seq[ChumashSpan.BookSpan] = getAliyot(isShabbos = false)

    def maftir: Option[ChumashSpan.BookSpan] = Some(metadata.maftir.get)

    def haftarah: Option[Haftarah] = Some(metadata.haftarah.get)
  }

  sealed trait WithSimpleReading extends WithReading with Loaded {
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

  sealed class Private(override val name: String) extends WithName with Loaded

  sealed trait FestivalOrIntermediate extends LoadedNames with Date with WithReading

  sealed trait Festival extends FestivalOrIntermediate with WithSimpleReading

  sealed trait SecondDay extends Festival with NonFirstDayOf {
    def secondDayOf: Festival

    override def maftir: Option[ChumashSpan.BookSpan] = secondDayOf.maftir
  }

  sealed abstract class Intermediate(override val name: String, intermediateDayNumber: Int, inHolyLand: Boolean)
    extends LoadedNames with FestivalOrIntermediate with NonFirstDayOf
  {
    override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)
  }

  sealed trait RabbinicFestival extends WithReading with Date

  sealed trait Fast extends WithSimpleReading with Date with NoShabbosAllowed { this: LoadedNames => }

  case object ShabbosErevRoshChodesh extends LoadedNames {
    override def name: String = "Shabbos Erev Rosh Chodesh"
  }

  case object RoshChodesh extends LoadedNames {
    override def name: String = "Rosh Chodesh"
  }

  private case object RoshChodeshPart2 extends LoadedNames {
    override def name: String = "Rosh Chodesh Part 2"
  }

  case object ShabbosRoshChodeshAdditionalHaftorah extends LoadedNames {
    override def name: String =  "Shabbos Rosh Chodesh Additional Haftorah"
  }

  case object ShabbosErevRoshChodeshAdditionalHaftorah extends LoadedNames {
    override def name: String =  "Shabbos Erev Rosh Chodesh Additional Haftorah"
  }

  sealed abstract class FastLike(override val name: String) extends LoadedNames with Fast with NoShabbosAllowed {
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

  case object RoshHashanah1 extends LoadedNames with Festival with FirstDayOf {
    override def name: String = "Rosh Hashanah 1"
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(1)
  }

  case object RoshHashanah2 extends LoadedNames
    with SecondDay with NonFirstDayOf with NoShabbosAllowed
  {
    override def name: String = "Rosh Hashanah 2"

    override def secondDayOf: Festival = RoshHashanah1

    override def firstDay: Date = RoshHashanah1

    override def dayNumber: Int = 2
  }

  case object FastOfGedalia extends FastLike("Fast of Gedalia") {
    final override def date(year: Year): Day = year.month(Tishrei).day(3)

    final override protected def correctDate(result: Day): Day =
      if (result.isShabbos) result+1 else result

    protected override def afternoonHaftarah: Option[Haftarah] = FastOfGedalia.haftarah
  }

  case object YomKippur extends LoadedNames with Festival {
    override def name: String = "Yom Kippur"

    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(10)

    override def getAfternoonReading: Option[Reading] = Some(Reading(
      aliyot = YomKippurAfternoon.getAliyot(false).init,
      maftir = Some(YomKippurAfternoon.getAliyot(false).last),
      haftarah = Some(YomKippurAfternoon.haftarah.get)
    ))
  }

  private case object YomKippurAfternoon extends Private("Yom Kippur Afternoon")

  case object Succos extends LoadedNames with Festival with FirstDayOf {
    override def name: String = "Succos"

    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(15)
  }

  case object Succos2 extends LoadedNames with SecondDay {
    override def name: String = "Succos 2"

    override def firstDay: Date = Succos

    override def dayNumber: Int = 2

    override def secondDayOf: Festival = Succos

    protected override def aliyotSameAs(isShabbos: Boolean): WithSimpleReading = Succos
  }

  // TODO mapping to the readings skips Shabbos - whatever that means... Or was it meant for Pesach only?
  sealed class SuccosIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate("Succos Intermediate " + intermediateDayNumber, intermediateDayNumber, inHolyLand)
      with WithSimpleReading
  {
    override def firstDay: Date = Succos

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

  case object SheminiAtzeres extends LoadedNames with Festival with NonFirstDayOf {
    override def name: String = "Shemini Atzeres"
    override def firstDay: Date = Succos
    override def dayNumber: Int = 8
  }

  case object SimchasTorah extends LoadedNames with SecondDay with NoShabbosAllowed {
    override def name: String = "Simchas Torah"

    override def firstDay: Date = Succos

    override def dayNumber: Int = 9

    override def secondDayOf: Festival = SheminiAtzeres

    override def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] =
      getAliyot(isShabbos) ++ SimchasTorahChassanBereishis.getAliyot(isShabbos)
  }

  private case object SimchasTorahChassanBereishis extends Private("Simchas Torah Chassan Bereishis")

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends LoadedNames
    with Festival with NonFirstDayOf
  {
    override def name: String = "Shemini Atzeres and Simchas Torah"

    override def firstDay: Date = Succos

    override def dayNumber: Int = 8

    override def getReading(isShabbos: Boolean): Reading = SimchasTorah.getReading(isShabbos)

    override def getAfternoonReading: Option[Reading] = SimchasTorah.getAfternoonReading
  }

  case object ShabbosBereishis extends LoadedNames with Date {
    override def name: String = "Shabbos Bereishis"
    override def date(year: Year): Day = shabbosAfter(SimchasTorah.date(year))
  }

  sealed class Chanukah(override val dayNumber: Int) extends LoadedNames
    with DayOf with RabbinicFestival
  {
    final override def name: String = "Chanukah " + dayNumber
    final override def firstDay: Date = Chanukah1

    override def date(year: Year): Day = Chanukah1.date(year)+(dayNumber-1)

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
    override def date(year: Year): Day = year.month(Kislev).day(25)
  }
  case object Chanukah2 extends Chanukah(2)
  case object Chanukah3 extends Chanukah(3)
  case object Chanukah4 extends Chanukah(4)
  case object Chanukah5 extends Chanukah(5)
  case object Chanukah6 extends Chanukah(6)
  case object Chanukah7 extends Chanukah(7)
  case object Chanukah8 extends Chanukah(8)

  case object FastOfTeves extends FastLike("Fast of 10th of Teves") {
    final def date(year: Year): Day = year.month(Teves).day(10)
  }

  // TODO FastLike
  case object FastOfEster extends LoadedNames with Fast {
    override def name: String = "fast of Ester"

    override def date(year: Year): Day = Purim.date(year)-1

    final override protected def correctDate(result: Day): Day =
      // If on Friday or Saturday - move to Thursday
      if (result.isShabbos) result-2 else
      if (result.next.isShabbos) result-1 else
        result
  }

  // TODO add in Schedule:

  case object ParshasShekalim extends LoadedNames with Date {
    override def name: String = "Parshas Shekalim"
    override def date(year: Year): Day = {
      val result = Purim.date(year).month.firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }
  }

  case object ParshasZachor extends LoadedNames with Date {
    override def name: String = "Parshas Zachor"
    override def date(year: Year): Day = shabbosBefore(Purim.date(year))
  }

  case object ParshasParah extends LoadedNames with Date {
    override def name: String = "Parshas Parah"
    override def date(year: Year): Day = shabbosBefore(ParshasHachodesh.date(year))
  }

  case object ParshasHachodesh extends LoadedNames with Date {
    override def name: String = "Parshas Hachodesh"

    override def date(year: Year): Day = {
      val result = year.month(Nisan).firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }
  }

  // TODO in Schedule
  case object ShabbosHagodol extends LoadedNames with Date {
    override def name: String = "Shabbos Hagodol"
    override def date(year: Year): Day = shabbosBefore(Pesach.date(year))
  }

  case object Purim extends LoadedNames with RabbinicFestival with WithSimpleReading with NoShabbosAllowed {
    override def name: String = "Purim"

    override def date(year: Year): Day = year.latestAdar.day(14)

    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = aliyot(isShabbos),
      maftir = None,
      haftarah = None
    )
  }
  case object ShushanPurim extends LoadedNames with Date {
    override def name: String = "Shushan Purim"
    override def date(year: Year): Day = Purim.date(year) + 1
  }

  case object Pesach extends LoadedNames with FirstDayOf with WithSimpleReading with Festival {
    override def name: String = "Pesach"
    final def date(year: Year): Day = year.month(Nisan).day(15)
  }

  case object Pesach2 extends LoadedNames with NonFirstDayOf with SecondDay with WithSimpleReading with NoShabbosAllowed {
    override def name: String = "Pesach 2"

    override def firstDay: Date = Pesach

    override def dayNumber: Int = 2

    override def secondDayOf: Festival = Pesach
  }

  sealed class PesachIntermediate(
    intermediateDayNumber: Int,
    inHolyLand: Boolean
  ) extends Intermediate("Pesach Intermediate " + intermediateDayNumber, intermediateDayNumber, inHolyLand) {
    override def firstDay: Date = Pesach

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


  case object Pesach7 extends LoadedNames with Festival with NonFirstDayOf with WithSimpleReading {
    override def name: String = "Pesach 7"

    override def firstDay: Date = Pesach

    override def dayNumber: Int = 7
  }

  case object Pesach8 extends LoadedNames with NonFirstDayOf with SecondDay {
    override def name: String = "Pesach 8"

    override def firstDay: Date = Pesach

    override def dayNumber: Int = 8

    override def secondDayOf: Festival = Pesach7

    override def aliyotSameAs(isShabbos: Boolean): WithSimpleReading =
      if (!isShabbos) this else SheminiAtzeres
  }

  case object LagBaOmer extends LoadedNames with Date {
    override def name: String = "Lag Ba Omer"
    override def date(year: Year): Day = year.month(Iyar).day(18)
  }

  case object Shavuos extends LoadedNames with FirstDayOf with Festival with NoShabbosAllowed {
    override def name: String = "Shavuos"
    override def date(year: Year): Day = year.month(Sivan).day(6)
  }

  case object Shavuos2 extends LoadedNames with NonFirstDayOf with SecondDay {
    override def name: String = "Shavuos 2"

    override def firstDay: Date = Shavuos

    override def dayNumber: Int = 2

    override def secondDayOf: Festival = Shavuos

    override def aliyotSameAs(isShabbos: Boolean): WithSimpleReading =
      if (!isShabbos) Pesach8 else SheminiAtzeres
  }

  case object FastOfTammuz extends FastLike("Fast of Tammuz") {
    override def date(year: Year): Day = year.month(Tammuz).day(17)

    final override protected def correctDate(result: Day): Day =
      if (result.isShabbos) result+1 else result
  }

  case object TishaBeAv extends LoadedNames with Fast with NoShabbosAllowed {
    override def name: String = "Tisha BeAv"

    override def date(year: Year): Day = year.month(Av).day(9)

    final override protected def correctDate(result: Day): Day =
      if (result.isShabbos) result+1 else result

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

  val daysWithSpecialReadingsNotFestivals: Set[Date with WithReading] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[WithReading] =
    Set.empty[WithReading] ++ festivals(inHolyLand) ++ fasts ++ rabbinicFestivals

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)

  // Needs to be lazy for initialization/metadata loading to work...
  private lazy val values: Seq[WithName] = Seq(
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

  private lazy val metadatas: Map[WithName, SpecialDayMetadata] = Metadata.loadMetadata(
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
