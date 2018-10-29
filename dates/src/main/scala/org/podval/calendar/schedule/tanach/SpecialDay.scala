package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.{Attributes, Metadata, Names, WithName, WithNames, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Aliyot, BookSpan, Custom, Span}
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.Jewish.Month.Name._
import org.podval.calendar.jewish.{JewishDay, JewishYear}

import scala.xml.Elem

// TODO synthesize names in known languages

object SpecialDay {

  sealed trait WithMetadata extends WithName {
    protected def metadata: SpecialDayMetadata = metadatas(this)
  }

  sealed trait LoadNames extends WithNames with WithMetadata {
    final override def names: Names = metadata.names
  }

  sealed trait Date {
    def date(year: Year): Day

    final def correctedDate(year: Year): Day = correctDate(date(year))

    protected def correctDate(date: Day): Day = date
  }

  sealed trait PostponeOnShabbos extends Date {
    final override protected def correctDate(result: Day): Day = if (result.isShabbos) result+1 else result
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

  sealed trait WithReading {
    def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading

    def getAfternoonReading: Option[Reading] = None
  }

  sealed trait SimpleReading extends WithReading {
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

    def isShabbosAllowed: Boolean = true

    def getReading(isShabbos: Boolean): Reading
  }

  sealed trait NoShabbosAllowed extends SimpleReading {
    final override def isShabbosAllowed: Boolean = false
  }

  sealed trait WithAliyot {
    def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan]
  }

  sealed trait AliyotSameAs extends WithAliyot {
    final override def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] = aliyotSameAs(isShabbos).aliyot(isShabbos)

    protected def aliyotSameAs(isShabbos: Boolean): VerySimpleReading
  }

  sealed trait LoadAliyot extends WithAliyot with WithMetadata {
    final override def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] = {
      val result = metadata
      (if (isShabbos) result.shabbosAliyot else result.weekdayAliyot).get.getAliyot
    }
  }

  sealed trait WithMaftir {
    def maftir: Option[ChumashSpan.BookSpan] = None
  }

  sealed trait LoadMaftir extends WithMaftir with WithMetadata {
    final override def maftir: Option[ChumashSpan.BookSpan] = Some(metadata.maftir.get)
  }

  sealed trait WithHaftarah {
    def haftarah: Option[Haftarah] = None
  }

  sealed trait LoadHaftarah extends WithHaftarah with WithMetadata {
    final override def haftarah: Option[Haftarah] = Some(metadata.haftarah.get)
  }

  sealed trait VerySimpleReading extends SimpleReading with WithAliyot with WithMaftir with WithHaftarah {
    final override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = aliyot(isShabbos),
      maftir = maftir,
      haftarah = haftarah
    )
  }

  sealed class Private(override val name: String) extends WithName

  sealed trait FestivalOrIntermediate extends WithNames with Date with WithReading

  sealed trait Festival extends FestivalOrIntermediate with SimpleReading

  sealed trait FestivalFirstDay extends Festival with VerySimpleReading
    with LoadNames with LoadAliyot with LoadMaftir with LoadHaftarah

  sealed trait FestivalSecondDay extends Festival with VerySimpleReading with NonFirstDayOf with LoadHaftarah {
    def secondDayOf: FestivalFirstDay

    override def maftir: Option[ChumashSpan.BookSpan] = secondDayOf.maftir
  }

  sealed abstract class Intermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends FestivalOrIntermediate with NonFirstDayOf
  {
    override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)
  }

  sealed trait RabbinicFestival extends WithNames with WithReading with Date

  sealed trait AfternoonReading extends WithReading {
    final override def getAfternoonReading: Option[Reading] =
      Some(readingWithLastAliyaAsMaftir(afternoonAliyot, afternoonHaftarah.get))

    protected def afternoonAliyot: Seq[ChumashSpan.BookSpan]

    protected def afternoonHaftarah: Option[Haftarah]
  }

  sealed trait Fast extends SimpleReading with Date with NoShabbosAllowed with AfternoonReading with LoadNames

  private case object ShabbosErevRoshChodesh extends Private("Shabbos Erev Rosh Chodesh")

  case object RoshChodesh extends LoadNames {
    override def name: String = "Rosh Chodesh"
  }

  private case object RoshChodeshPart2 extends Private("Rosh Chodesh Part 2")
  private case object ShabbosRoshChodeshAdditionalHaftorah extends Private("Shabbos Rosh Chodesh Additional Haftorah")
  private case object ShabbosErevRoshChodeshAdditionalHaftorah extends Private("Shabbos Erev Rosh Chodesh Additional Haftorah")

  sealed abstract class FastLike(override val name: String) extends Fast with VerySimpleReading {
    final override def aliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] =
      FastReading.aliyot(isShabbos) ++ FastPart2.aliyot(isShabbos)

    final override def maftir: Option[BookSpan.ChumashSpan.BookSpan] = None

    /* TODO deal with fast of Gedalia and make final*/ override def haftarah: Option[Haftarah] = None

    final override protected def afternoonAliyot: Seq[ChumashSpan.BookSpan] = aliyot(false)

    override protected def afternoonHaftarah: Option[Haftarah] = FastPart2.haftarah
  }

  private case object FastReading extends Private("Fast") with LoadAliyot

  private case object FastPart2 extends Private("Fast Part 2") with LoadAliyot with LoadHaftarah

  private case object IntermediateShabbos extends Private("Intermediate Shabbos") with LoadAliyot

  case object RoshHashanah1 extends FestivalFirstDay with FirstDayOf {
    override def name: String = "Rosh Hashanah 1"
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(1)
  }

  case object RoshHashanah2 extends FestivalSecondDay with NoShabbosAllowed with LoadAliyot {
    override def name: String = "Rosh Hashanah 2"
    override def names: Names = ???
    override def secondDayOf: FestivalFirstDay = RoshHashanah1
    override def firstDay: Date = RoshHashanah1
    override def dayNumber: Int = 2
  }

  // TODO what is the maftir for those that do read haftarah
  case object FastOfGedalia extends FastLike("Fast of Gedalia")
    with PostponeOnShabbos with LoadHaftarah with AfternoonReading
  {
    final override def date(year: Year): Day = year.month(Tishrei).day(3)
    protected override def afternoonHaftarah: Option[Haftarah] = haftarah
  }

  case object YomKippur extends FestivalFirstDay with AfternoonReading {
    override def name: String = "Yom Kippur"
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(10)
    override protected def afternoonAliyot: Seq[ChumashSpan.BookSpan] = YomKippurAfternoon.aliyot(false)
    override protected def afternoonHaftarah: Option[Haftarah] = YomKippurAfternoon.haftarah
  }

  private case object YomKippurAfternoon extends Private("Yom Kippur Afternoon") with LoadAliyot with LoadHaftarah

  case object Succos extends FestivalFirstDay with FirstDayOf {
    override def name: String = "Succos"
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(15)
  }

  case object Succos2 extends FestivalSecondDay with AliyotSameAs {
    override def name: String = "Succos 2"
    override def names: Names = ???
    override def firstDay: Date = Succos
    override def dayNumber: Int = 2
    override def secondDayOf: FestivalFirstDay = Succos
    protected override def aliyotSameAs(isShabbos: Boolean): VerySimpleReading = Succos
  }

  // TODO mapping to the readings skips Shabbos - whatever that means... Or was it meant for Pesach only?
  sealed class SuccosIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand) with SimpleReading
  {
    override def names: Names = ???

    override def firstDay: Date = Succos

    override def getReading(isShabbos: Boolean): Reading = {
      val all: Seq[ChumashSpan.BookSpan] = SuccosIntermediateReading.aliyot(false)
      def korbanot(n: Int): ChumashSpan.BookSpan = all(n-1)

      if (intermediateDayNumber == 6) require(inHolyLand)
      val last: ChumashSpan.BookSpan =
        if (inHolyLand) korbanot(intermediateDayNumber)
        else korbanot(intermediateDayNumber) + korbanot(intermediateDayNumber+1)

      if (isShabbos) Reading(
        aliyot = IntermediateShabbos.aliyot(isShabbos),
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

  private case object SuccosIntermediateReading extends Private("Succos Intermediate") with LoadAliyot with LoadHaftarah

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

  case object SheminiAtzeres extends FestivalFirstDay with NonFirstDayOf {
    override def name: String = "Shemini Atzeres"
    override def firstDay: Date = Succos
    override def dayNumber: Int = 8
  }

  case object SimchasTorah extends Festival with NonFirstDayOf with NoShabbosAllowed
    with LoadNames with LoadAliyot with LoadHaftarah
  {
    override def name: String = "Simchas Torah"

    override def firstDay: Date = Succos

    override def dayNumber: Int = 9

    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = aliyot(isShabbos) ++ SimchasTorahChassanBereishis.aliyot(isShabbos),
      maftir = SheminiAtzeres.maftir,
      haftarah = haftarah
    )
  }

  private case object SimchasTorahChassanBereishis extends Private("Simchas Torah Chassan Bereishis") with LoadAliyot

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends Festival with NonFirstDayOf with LoadNames {
    override def name: String = "Shemini Atzeres and Simchas Torah"
    override def firstDay: Date = Succos
    override def dayNumber: Int = 8
    override def getReading(isShabbos: Boolean): Reading = SimchasTorah.getReading(isShabbos)
    override def getAfternoonReading: Option[Reading] = SimchasTorah.getAfternoonReading
  }

  case object ShabbosBereishis extends LoadNames with Date {
    override def name: String = "Shabbos Bereishis"
    override def date(year: Year): Day = shabbosAfter(SimchasTorah.date(year))
  }

  sealed class Chanukah(override val dayNumber: Int) extends WithNames with DayOf with RabbinicFestival {
    override def names: Names = ???
//    final override def name: String = "Chanukah " + dayNumber
    final override def firstDay: Date = Chanukah1

    override def date(year: Year): Day = Chanukah1.date(year)+(dayNumber-1)

    final override def getReading(
      isShabbos: Boolean,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading = {
      val all: Seq[ChumashSpan.BookSpan] = ChannukahReading.aliyot(false)
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
            val endAliyot = ChannukahEnd.aliyot(false)
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

  private case object ChannukahReading extends Private("Channukah") with LoadAliyot
  private case object ChannukahEnd extends Private("Channukah End") with LoadAliyot

  private case object ChannukahShabbos1 extends Private("Channukah Shabbos 1") with LoadHaftarah
  private case object ChannukahShabbos2 extends Private("Channukah Shabbos 2") with LoadHaftarah

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

  case object FastOfEster extends FastLike("Fast of Ester") {
    override def date(year: Year): Day = Purim.date(year)-1

    final override protected def correctDate(result: Day): Day =
      // If on Friday or Saturday - move to Thursday
      if (result.isShabbos) result-2 else
      if (result.next.isShabbos) result-1 else
        result
  }


  sealed abstract class SpecialParsha(override val name: String) extends Date with LoadNames with LoadMaftir with LoadHaftarah

  // TODO add in Schedule:

  case object ParshasShekalim extends SpecialParsha("Parshas Shekalim") {
    override def date(year: Year): Day = {
      val result = Purim.date(year).month.firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }
  }

  case object ParshasZachor extends SpecialParsha("Parshas Zachor") {
    override def date(year: Year): Day = shabbosBefore(Purim.date(year))
  }

  case object ParshasParah extends SpecialParsha("Parshas Parah") {
    override def date(year: Year): Day = shabbosBefore(ParshasHachodesh.date(year))
  }

  case object ParshasHachodesh extends SpecialParsha("Parshas Hachodesh") {
    override def date(year: Year): Day = {
      val result = year.month(Nisan).firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }
  }

  case object ShabbosHagodol extends LoadNames with Date {
    override def name: String = "Shabbos Hagodol"
    override def date(year: Year): Day = shabbosBefore(Pesach.date(year))
  }

  case object Purim extends LoadNames with RabbinicFestival with VerySimpleReading with NoShabbosAllowed with LoadAliyot {
    override def name: String = "Purim"
    override def date(year: Year): Day = year.latestAdar.day(14)
  }
  case object ShushanPurim extends LoadNames with Date {
    override def name: String = "Shushan Purim"
    override def date(year: Year): Day = Purim.date(year) + 1
  }

  case object Pesach extends FestivalFirstDay with FirstDayOf {
    override def name: String = "Pesach"
    final def date(year: Year): Day = year.month(Nisan).day(15)
  }

  case object Pesach2 extends FestivalSecondDay with NonFirstDayOf with NoShabbosAllowed with LoadAliyot {
    override def name: String = "Pesach 2"
    override def names: Names = ???
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 2
    override def secondDayOf: FestivalFirstDay = Pesach
  }

  sealed class PesachIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand)
  {
    override def names: Names = ???

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
          aliyot = IntermediateShabbos.aliyot(isShabbos),
          maftir = Pesach7.maftir,
          haftarah = PesachIntermediateShabbos.haftarah
        )
      } else {
        val reading: LoadAliyot = dayNumber match {
          case 2 => Pesach2InHolyLand
          case 3 => Pesach3
          case 4 => if (!isPesachOnChamishi) Pesach4 else Pesach3
          case 5 => if (isShabbos || !isPesachOnChamishi) Pesach5 else Pesach4
          case 6 => Pesach6
        }
        Reading(
          aliyot = reading.aliyot(isShabbos) /* TODO: ? */ :+ Pesach7.maftir.get,
          maftir = None,
          haftarah = None
        )
      }
    }
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


  private case object PesachIntermediateShabbos extends Private("Pesach Intermediate Shabbos") with LoadHaftarah
  private case object Pesach2InHolyLand extends Private("Pesach 2 in Holy Land") with LoadAliyot
  private case object Pesach3 extends Private("Pesach 3") with LoadAliyot
  private case object Pesach4 extends Private("Pesach 4") with LoadAliyot
  private case object Pesach5 extends Private("Pesach 5") with LoadAliyot
  private case object Pesach6 extends Private("Pesach 6") with LoadAliyot

  case object Pesach7 extends FestivalFirstDay with NonFirstDayOf {
    override def name: String = "Pesach 7"
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 7
  }

  case object Pesach8 extends FestivalSecondDay with NonFirstDayOf with LoadNames with AliyotSameAs {
    override def name: String = "Pesach 8"
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 8
    override def secondDayOf: FestivalFirstDay = Pesach7

    protected override def aliyotSameAs(isShabbos: Boolean): VerySimpleReading =
      if (!isShabbos) this else SheminiAtzeres
  }

  case object LagBaOmer extends LoadNames with Date {
    override def name: String = "Lag Ba Omer"
    override def date(year: Year): Day = year.month(Iyar).day(18)
  }

  case object Shavuos extends FestivalFirstDay with FirstDayOf with NoShabbosAllowed {
    override def name: String = "Shavuos"
    override def date(year: Year): Day = year.month(Sivan).day(6)
  }

  case object Shavuos2 extends FestivalSecondDay with NonFirstDayOf with AliyotSameAs {
    override def name: String = "Shavuos 2"
    override def names: Names = ???
    override def firstDay: Date = Shavuos
    override def dayNumber: Int = 2
    override def secondDayOf: FestivalFirstDay = Shavuos

    protected override def aliyotSameAs(isShabbos: Boolean): VerySimpleReading =
      if (!isShabbos) Pesach8 else SheminiAtzeres
  }

  case object FastOfTammuz extends FastLike("Fast of Tammuz") with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tammuz).day(17)
  }

  case object TishaBeAv extends Fast with NoShabbosAllowed with LoadAliyot with PostponeOnShabbos with LoadHaftarah {
    override def name: String = "Tisha BeAv"
    override def date(year: Year): Day = year.month(Av).day(9)
    override def getReading(isShabbos: Boolean): Reading = readingWithLastAliyaAsMaftir(aliyot(isShabbos), haftarah.get)
    override protected def afternoonAliyot: Seq[ChumashSpan.BookSpan] = aliyot(false)
    override def afternoonHaftarah: Option[Haftarah] = TishaBeAvAfternoon.haftarah
  }

  private case object TishaBeAvAfternoon extends Private("Tisha BeAv Afternoon") with LoadHaftarah

  private def readingWithLastAliyaAsMaftir(aliyot: Seq[ChumashSpan.BookSpan], haftarah: Haftarah): Reading = Reading(
    aliyot = aliyot.init,
    maftir = Some(aliyot.last),
    haftarah = Some(haftarah)
  )

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
