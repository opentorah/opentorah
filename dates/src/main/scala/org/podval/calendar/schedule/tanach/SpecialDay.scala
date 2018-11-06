package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.{Attributes, Names, WithNames, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Aliyot, BookSpan, Custom, Span}
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.Jewish.Month.Name._
import org.podval.calendar.jewish.{JewishDay, JewishYear}

import scala.xml.Elem

// TODO synthesize names in known languages

object SpecialDay {

  type Torah = Seq[ChumashSpan.BookSpan]

  sealed trait Date {
    def date(year: Year): Day

    final def correctedDate(year: Year): Day = correctDate(date(year))

    protected def correctDate(date: Day): Day = date
  }

  sealed trait PostponeOnShabbos extends Date with NoShabbosAllowed {
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

  // TODO number of aliyot: Shabbos and Simchas Torah - 7; Yom Kippur - 6 (even on Shabbos); Yom Tov - 5;
  // Intermediate and Rosh Chodesh - 4; Chanukkah, Purim, Fast, Sheni/Chamishi and Shabbos afternoon - 3.
  sealed trait WithReading {
    def getReading(
      isShabbos: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading

    def getAfternoonReading: Option[Reading] = None
  }

  sealed trait SimpleReading extends WithReading {
    final override def getReading(
      isShabbos: Boolean,
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

  sealed trait LoadNames extends WithNames {
    protected val namesXml: Elem

    final override lazy val names: Names = Names.parse(namesXml, None)
  }

  sealed trait WithAliyot {
    def aliyot(isShabbos: Boolean): Torah
  }

  sealed trait LoadAliyotForShabbosAndWeekday extends WithAliyot {
    protected val torahXml: Elem

    private lazy val (shabbosAliyot: Torah, weekdayAliyot: Torah) = parseTorahForShabbosAndWeekday(torahXml)

    final override def aliyot(isShabbos: Boolean): Torah = if (isShabbos) shabbosAliyot else weekdayAliyot
  }

  sealed trait LoadAliyot extends WithAliyot {
    protected val torahXml: Elem

    private lazy val torah: Torah = parseTorah(torahXml)

    final override def aliyot(isShabbos: Boolean): Torah = torah
  }

  sealed trait WithMaftir {
    def maftir: Option[ChumashSpan.BookSpan] = None
  }

  sealed trait LoadMaftir extends WithMaftir {
    protected val maftirXml: Elem

    final override lazy val maftir: Option[ChumashSpan.BookSpan] = Some(parseMaftir(maftirXml))
  }

  sealed trait WithHaftarah {
    def haftarah: Option[Haftarah.OptionalCustoms] = None
  }

  sealed trait LoadHaftarah extends WithHaftarah {
    protected val haftarahXml: Elem

    final override lazy val haftarah: Option[Haftarah.OptionalCustoms] = Some(parseHaftarah(haftarahXml))
  }

  sealed trait VerySimpleReading extends SimpleReading with WithAliyot with WithMaftir with WithHaftarah {
    final override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = aliyot(isShabbos),
      maftir = maftir,
      haftarah = haftarah
    )
  }

  sealed trait FestivalOrIntermediate extends WithNames with Date with WithReading

  sealed trait Festival extends FestivalOrIntermediate with SimpleReading with WithMaftir

  sealed trait FestivalFirstDay extends Festival with VerySimpleReading
    with LoadNames with LoadMaftir with LoadHaftarah

  sealed trait FestivalSecondDay extends Festival with VerySimpleReading with NonFirstDayOf with LoadHaftarah {
    def secondDayOf: Festival

    final override def maftir: Option[ChumashSpan.BookSpan] = secondDayOf.maftir
  }

  sealed abstract class Intermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends FestivalOrIntermediate with NonFirstDayOf
  {
    final override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)

    protected final val shabbosTorah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="33" fromVerse="12" toChapter="34" toVerse="26">
        <aliyah n="2" fromChapter="33" fromVerse="17"/>
        <aliyah n="3" fromChapter="33" fromVerse="20"/>
        <aliyah n="4" fromChapter="34" fromVerse="1"/>
        <aliyah n="5" fromChapter="34" fromVerse="4"/>
        <aliyah n="6" fromChapter="34" fromVerse="11"/>
        <aliyah n="7" fromChapter="34" fromVerse="18"/>
      </torah>)
  }

  sealed trait RabbinicFestival extends WithNames with WithReading with Date

  sealed trait AfternoonReading extends WithReading {
    final override def getAfternoonReading: Option[Reading] =
      Some(readingWithLastAliyaAsMaftir(afternoonAliyot, afternoonHaftarah.get))

    protected def afternoonAliyot: Torah

    protected def afternoonHaftarah: Option[Haftarah.OptionalCustoms]
  }

  case object RoshChodesh extends LoadNames {
    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Rosh Chodesh</name>
      </names>

    private val torah: Torah = parseTorah(
      <torah book="Numbers" fromChapter="28" fromVerse="1" toVerse="15">
        <aliyah n="2" fromVerse="3"/>
        <aliyah n="3" fromVerse="4"/>
        <aliyah n="4" fromVerse="6"/>
        <aliyah n="5" fromVerse="9"/>
        <aliyah n="6" fromVerse="11"/>
      </torah>)

    def weekday: Reading = Reading(
      aliyot = Seq(torah(0)+torah(1), torah(1)+torah(2), torah(3)+torah(4), torah(5)),
      maftir = None,
      haftarah = None
    )

    def shabbos(weeklyReading: Option[WeeklyReading]): Reading = Reading(
      aliyot = Seq(), //TODO: aliyot from the weekly reading
      maftir = Some(shabbosMaftir),
      haftarah = Some(shabbosHaftarah)
    )

    def in3aliyot: Torah = Seq(torah(0)+torah(1)+torah(2), torah(3)+torah(4), torah(5))

    def shabbosMaftir: ChumashSpan.BookSpan = torah(4)+torah(5)

    private val shabbosHaftarah: Haftarah.OptionalCustoms = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="66">
        <part n="1" fromVerse="1" toVerse="24"/>
        <part n="2" fromVerse="23" toVerse="23"/>
      </haftarah>)

    // TODO added by Chabad only
    private val shabbosAdditionalHaftarah: Haftarah.OptionalCustoms = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="66">
        <part n="1" fromVerse="1" toVerse="1"/>
        <part n="2" fromVerse="23" toVerse="24"/>
        <part n="3" fromVerse="23" toVerse="23"/>
      </haftarah>)

    private val shabbosErevRoshChodeshHaftarah: Haftarah.OptionalCustoms = parseHaftarah(
      <haftarah book="I Samuel" fromChapter="20" fromVerse="18" toVerse="42"/>)

    // TODO added by Chabad and Fes only; after haftarah if it wasn't pushed off by the haftarah Shabbos erev Rosh Chodesh.
    private val shabbosErevRoshChodeshAdditionalHaftorah: Haftarah.OptionalCustoms = parseHaftarah(
      <haftarah book="I Samuel" fromChapter="20">
        <part n="1" fromVerse="18" toVerse="18"/>
        <part n="2" fromVerse="42" toVerse="42"/>
      </haftarah>)
  }

  sealed trait Fast extends SimpleReading with Date with NoShabbosAllowed with AfternoonReading with LoadNames {
    final override protected def afternoonAliyot: Torah = afternoonTorah

    protected final val afternoonTorah: Torah = parseTorah(
        <torah book="Exodus" fromChapter="32" fromVerse="11" toVerse="14"/>) ++ parseTorah(
      <torah book="Exodus" fromChapter="34" fromVerse="1" toVerse="10">
        <aliyah n="2" fromVerse="4"/>
      </torah>)

    final override protected def afternoonHaftarah: Option[Haftarah.OptionalCustoms] = Some(defaultHaftarah) // TODO apply haftarahExceptions

    private val defaultHaftarah: Haftarah.OptionalCustoms = parseHaftarah(
      <haftarah>
        <custom n="Common" empty="true"/>
        <custom n="Ashkenaz, Chabad, Morocco" book="Isaiah" fromChapter="55" fromVerse="6" toChapter="56" toVerse="8"/>
        <custom n="Algeria">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>)

    protected val haftarahExceptions: Option[Haftarah.OptionalCustoms] = None
  }

  sealed trait NonTishaBeAvFast extends Fast {
    final override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = afternoonTorah,
      maftir = None,
      haftarah = None
    )
  }

  case object RoshHashanah1 extends FestivalFirstDay with FirstDayOf with LoadAliyotForShabbosAndWeekday {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(1)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Rosh Hashanah</name>
      </names>

    protected override val torahXml: Elem =
      <torah book="Genesis" fromChapter="21" fromVerse="1" toVerse="34">
        <aliyah n="2" fromVerse="5"/>
        <aliyah n="3" fromVerse="9" shabbos="true"/>
        <aliyah n="4" fromVerse="13"/>
        <aliyah n="5" fromVerse="18" shabbos="true"/>
        <aliyah n="6" fromVerse="22"/>
        <aliyah n="7" fromVerse="28"/>
      </torah>

    protected override val maftirXml: Elem =
        <maftir book="Numbers" fromChapter="29" fromVerse="1" toVerse="6"/>

    protected override val haftarahXml: Elem =
        <haftarah book="I Samuel" fromChapter="1" fromVerse="1" toChapter="2" toVerse="10"/>
  }

  case object RoshHashanah2 extends FestivalSecondDay with NoShabbosAllowed with LoadAliyot {
    override def names: Names = ???
    override def secondDayOf: FestivalFirstDay = RoshHashanah1
    override def firstDay: Date = RoshHashanah1
    override def dayNumber: Int = 2

    protected override val torahXml: Elem =
      <torah book="Genesis" fromChapter="22" fromVerse="1" toVerse="24">
        <aliyah n="2" fromVerse="4"/>
        <aliyah n="3" fromVerse="9"/>
        <aliyah n="4" fromVerse="15"/>
        <aliyah n="5" fromVerse="20"/>
      </torah>

    protected override val haftarahXml: Elem =
      <haftarah book="Jeremiah" fromChapter="31" fromVerse="1" toVerse="19"/>
  }

  case object FastOfGedalia extends NonTishaBeAvFast with PostponeOnShabbos with AfternoonReading {
    final override def date(year: Year): Day = year.month(Tishrei).day(3)

    protected override val namesXml: Elem =
      <names>
        <name lang="en">Fast of Gedalia</name>
      </names>

    protected override val haftarahExceptions: Option[Haftarah.OptionalCustoms] = Some(parseHaftarahNotFull(
      <haftarah>
        <custom n="Morocco">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>))
  }

  case object YomKippur extends FestivalFirstDay with AfternoonReading with LoadAliyotForShabbosAndWeekday {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(10)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Yom Kippur</name>
      </names>

    protected override val torahXml: Elem =
      <torah book="Leviticus" fromChapter="16" fromVerse="1" toVerse="34">
        <aliyah n="2" fromVerse="4" shabbos="true"/>
        <aliyah n="3" fromVerse="7"/>
        <aliyah n="4" fromVerse="12"/>
        <aliyah n="5" fromVerse="18"/>
        <aliyah n="6" fromVerse="25"/>
        <aliyah n="7" fromVerse="31"/>
      </torah>

    protected override val maftirXml: Elem =
        <maftir book="Numbers" fromChapter="29" fromVerse="7" toVerse="11"/>

    protected override val haftarahXml: Elem =
      <haftarah book="Isaiah">
        <custom n="Common" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
        <custom n="Italki, Teiman">
          <part n="1" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
          <part n="2" fromChapter="59" fromVerse="20" toVerse="21"/>
        </custom>
      </haftarah>

    override protected val afternoonAliyot: Torah = parseTorah(
      <torah book="Leviticus" fromChapter="18" fromVerse="1" toVerse="30">
        <aliyah n="2" fromVerse="6"/>
        <aliyah n="3" fromVerse="22"/>
      </torah>)

    override protected val afternoonHaftarah: Option[Haftarah.OptionalCustoms] = Some(parseHaftarah(
      <haftarah>
        <custom n="Common">
          <part n="1" book="Jonah" fromChapter="1" fromVerse="1" toChapter="4" toVerse="11"/>
          <part n="2" book="Micah" fromChapter="7" fromVerse="18" toVerse="20"/>
        </custom>
        <custom n="Italki">
          <part n="1" book="Obadiah" fromChapter="1" fromVerse="21"/>
          <part n="2" book="Jonah" fromChapter="1" fromVerse="1" toChapter="4" toVerse="11"/>
          <part n="3" book="Micah" fromChapter="7" fromVerse="18" toVerse="20"/>
        </custom>
      </haftarah>))
  }

  case object Succos extends Festival with FirstDayOf with VerySimpleReading
    with LoadNames with LoadAliyotForShabbosAndWeekday with LoadHaftarah
  {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(15)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Succos</name>
      </names>

    protected override val torahXml: Elem =
      <torah book="Leviticus" fromChapter="22" fromVerse="26" toChapter="23" toVerse="44">
        <aliyah n="2" fromChapter="23" fromVerse="1" shabbos="true"/>
        <aliyah n="3" fromChapter="23" fromVerse="4"/>
        <aliyah n="4" fromChapter="23" fromVerse="9" shabbos="true"/>
        <aliyah n="5" fromChapter="23" fromVerse="15"/>
        <aliyah n="6" fromChapter="23" fromVerse="23"/>
        <aliyah n="7" fromChapter="23" fromVerse="33"/>
      </torah>

    override def maftir: Option[BookSpan.ChumashSpan.BookSpan] = Some(korbanot.head)

    protected override val haftarahXml: Elem =
      <haftarah book="Zechariah" toChapter="14" toVerse="21">
        <custom n="Common" fromChapter="14" fromVerse="1"/>
        <custom n="Teiman" fromChapter="13" fromVerse="9"/>
      </haftarah>

    val korbanot: Torah = parseTorah(
      <torah book="Numbers" fromChapter="29" fromVerse="12" toVerse="34">
        <aliyah n="2" fromVerse="17"/>
        <aliyah n="3" fromVerse="20"/>
        <aliyah n="4" fromVerse="23"/>
        <aliyah n="5" fromVerse="26"/>
        <aliyah n="6" fromVerse="29"/>
        <aliyah n="7" fromVerse="32"/>
      </torah>)
  }

  case object Succos2 extends FestivalSecondDay {
    override def names: Names = ???
    override def firstDay: Date = Succos
    override def dayNumber: Int = 2
    override def secondDayOf: Festival = Succos

    override def aliyot(isShabbos: Boolean): Torah = Succos.aliyot(isShabbos)

    protected override val haftarahXml: Elem =
      <haftarah book="I Kings">
        <custom n="Common" fromChapter="8" fromVerse="2" toVerse="21"/>
        <custom n="Italki" fromChapter="7" fromVerse="51" toChapter="8" toVerse="15"/>
        <custom n="Teiman" fromChapter="7" fromVerse="51" toChapter="8" toVerse="21"/>
      </haftarah>
  }

  sealed class SuccosIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand) with SimpleReading with LoadHaftarah
  {
    override def names: Names = ???

    override def firstDay: Date = Succos

    override def getReading(isShabbos: Boolean): Reading = {
      def korbanot(n: Int): ChumashSpan.BookSpan = Succos.korbanot(n-1)

      if (intermediateDayNumber == 6) require(inHolyLand)
      val last: ChumashSpan.BookSpan =
        if (inHolyLand) korbanot(dayNumber) else korbanot(dayNumber-1) + korbanot(dayNumber)

      if (isShabbos) Reading(
        aliyot = shabbosTorah,
        maftir = Some(last),
        haftarah = haftarah
      ) else {
        val n: Int = Math.min(dayNumber, 5)

        val aliyot: Custom.Of[Torah] = Map(
          Custom.Common -> (Seq(korbanot(n), korbanot(n+1), korbanot(n+2)) :+ last),
          Custom.RavNaeHolyLand -> Seq(korbanot(n), korbanot(n), korbanot(n), korbanot(n))
        )

        Reading(aliyot, None, None)
      }
    }

    protected override val haftarahXml: Elem =
      <haftarah book="Ezekiel" fromChapter="38">
        <custom n="Common" fromVerse="18" toChapter="39" toVerse="16"/>
        <custom n="Italki, Teiman" fromVerse="1" toChapter="38" toVerse="23"/>
      </haftarah>
  }

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

  case object SheminiAtzeres extends FestivalFirstDay with NonFirstDayOf with LoadAliyotForShabbosAndWeekday {
    override def firstDay: Date = Succos
    override def dayNumber: Int = 8

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Shemini Atzeres</name>
      </names>

    override protected val torahXml: Elem =
      <torah book="Deuteronomy" fromChapter="14" fromVerse="22" toChapter="16" toVerse="17">
        <aliyah n="2" fromChapter="15" fromVerse="1" shabbos="true"/>
        <aliyah n="3" fromChapter="15" fromVerse="19" shabbos="true"/>
        <aliyah n="4" fromChapter="16" fromVerse="1"/>
        <aliyah n="5" fromChapter="16" fromVerse="4"/>
        <aliyah n="6" fromChapter="16" fromVerse="9"/>
        <aliyah n="7" fromChapter="16" fromVerse="13"/>
      </torah>

    def shortAliyot: Torah = aliyot(isShabbos = true).drop(2)

    override protected val maftirXml: Elem =
        <maftir book="Numbers" fromChapter="29" fromVerse="35" toChapter="30" toVerse="1"/>

    override protected val haftarahXml: Elem =
    /*
  Artscroll gives custom Ashkenaz ending at 9:1,
  but @michaelko58 insists that it is the same as Sefard and ends at 8:66.
  His explanation: "there are some ashkenazic communities that follow custom Italki.
  It is possible that this is a difference between chassidim and litaim."
  */
      <haftarah book="I Kings" fromChapter="8" fromVerse="54">
        <custom n="Common" toChapter="8" toVerse="66"/>
        <custom n="Italki, Chabad" toChapter="9" toVerse="1"/>
      </haftarah>
  }

  case object SimchasTorah extends Festival with NonFirstDayOf with NoShabbosAllowed
    with LoadNames with LoadHaftarah
  {
    override def firstDay: Date = Succos

    override def dayNumber: Int = 9

    // Although Simchas Torah doesn't fall on Shabbos, its aliyot are reused by SheminiAtzeresAndSimchasTorahInHolyLand,
    // which does; so, isShabbos has to explicitly be set to false:
    override def getReading(isShabbos: Boolean): Reading = {
      Reading(
        aliyot = torah,
        maftir = SheminiAtzeres.maftir,
        haftarah = haftarah
      )
    }

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Simchas Torah</name>
      </names>

    // Zos Haberacha
    private val torah: Torah = parseTorah(
      <torah book="Deuteronomy" fromChapter="33" fromVerse="1" toChapter="34" toVerse="12">
        <aliyah n="2" fromChapter="33" fromVerse="5"/>
        <aliyah n="3" fromChapter="33" fromVerse="13"/>
        <aliyah n="4" fromChapter="33" fromVerse="18"/>
        <aliyah n="5" fromChapter="33" fromVerse="22"/>
        <aliyah n="6" fromChapter="33" fromVerse="27"/>
      </torah>) ++ parseTorah(
        <torah book="Genesis" fromChapter="1" fromVerse="1" toChapter="2" toVerse="3"/>)

    protected override val haftarahXml: Elem =
      <haftarah book="Joshua">
        <custom n="Common, Italki" fromChapter="1" fromVerse="1" toVerse="18"/>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="9"/>
          <part n="2" fromChapter="6" fromVerse="27"/>
        </custom>
      </haftarah>
  }

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends Festival with NonFirstDayOf with LoadNames {
    override def firstDay: Date = Succos
    override def dayNumber: Int = 8
    override def getReading(isShabbos: Boolean): Reading = SimchasTorah.getReading(isShabbos)
    override def getAfternoonReading: Option[Reading] = SimchasTorah.getAfternoonReading

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Shemini Atzeres and Simchas Torah</name>
      </names>
  }

  case object ShabbosBereishis extends LoadNames with Date {
    override def date(year: Year): Day = shabbosAfter(SimchasTorah.date(year))

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Shabbos Bereishis</name>
      </names>
  }

  sealed class Chanukah(override val dayNumber: Int) extends WithNames with DayOf with RabbinicFestival {
    override def names: Names = ???
    final override def firstDay: Date = Chanukah1

    override def date(year: Year): Day = Chanukah1.date(year)+(dayNumber-1)

    final override def getReading(
      isShabbos: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading = {
      if (isShabbos) require(weeklyReading.isDefined)

      def first(n: Int): ChumashSpan.BookSpan = torah(2*n-1)
      def second(n: Int): ChumashSpan.BookSpan = torah(2*n  )
      def full(n: Int): ChumashSpan.BookSpan = first(n)+second(n)

      if (isShabbos) Reading(
        aliyot = weeklyReading.get.getReading.aliyot,
        maftir = Some(full(dayNumber)),
        haftarah = Some(if (dayNumber < 8) haftarahShabbos1 else haftarahShabbos2)
      ) else {
        val (common: Torah, ashkenazAndChabadTail: Torah, sefardTail: Torah) =
          if (dayNumber == 1) (
            Seq(torah.head),
            Seq(first(dayNumber), second(dayNumber)),
            Seq(full(dayNumber), full(dayNumber + 1))
          ) else (
            // TODO it seems that for 1 < dayNumber < 8, there is no difference between the customs?!
            Seq(first(dayNumber), second(dayNumber)),
            Seq(if (dayNumber == 8) torah.last else full(dayNumber)),
            Seq(if (dayNumber == 8) full(dayNumber) + torah.last else full(dayNumber))
          )

        val ashkenazAndChabad = common ++ ashkenazAndChabadTail
        val sefard = common ++ sefardTail
        require(ashkenazAndChabad.length == 3)
        require(sefard.length == 3)

        val result: Custom.Of[Torah] = Map(
          Custom.Ashkenaz -> ashkenazAndChabad,
          Custom.Chabad -> ashkenazAndChabad,
          Custom.Sefard -> sefard
        )

        Reading(
          aliyot = result,
          maftir = None,
          haftarah = None
        )
      }
    }

    // last aliyah is for Zos Channukah
    private val torah: Torah = parseTorah(
      <torah book="Numbers" fromChapter="7" fromVerse="1" toChapter="8" toVerse="4">
        <aliyah n="1"  fromVerse="1" />
        <aliyah n="2"  fromVerse="12"/>
        <aliyah n="3"  fromVerse="15"/>
        <aliyah n="4"  fromVerse="18"/>
        <aliyah n="5"  fromVerse="21"/>
        <aliyah n="6"  fromVerse="24"/>
        <aliyah n="7"  fromVerse="27"/>
        <aliyah n="8"  fromVerse="30"/>
        <aliyah n="9"  fromVerse="33"/>
        <aliyah n="10" fromVerse="36"/>
        <aliyah n="11" fromVerse="39"/>
        <aliyah n="12" fromVerse="42"/>
        <aliyah n="13" fromVerse="45"/>
        <aliyah n="14" fromVerse="48"/>
        <aliyah n="15" fromVerse="51"/>
        <aliyah n="16" fromVerse="54"/>
        <aliyah n="17" fromVerse="57"/>
        <aliyah n="18" fromVerse="60"/>
      </torah>)

    private val haftarahShabbos1: Haftarah.OptionalCustoms = parseHaftarah(
        <haftarah book="Zechariah" fromChapter="2" fromVerse="14" toChapter="4" toVerse="7"/>)

    private val haftarahShabbos2: Haftarah.OptionalCustoms = parseHaftarah(
      <haftarah book="I Kings" fromChapter="7" fromVerse="40" toChapter="7">
        <custom n="Common" toVerse="50"/>
        <custom n="Italki" toVerse="51"/>
      </haftarah>)
  }

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

  case object FastOfTeves extends NonTishaBeAvFast {
    final def date(year: Year): Day = year.month(Teves).day(10)

    protected override val namesXml: Elem =
      <names>
        <name lang="en">Fast of 10th of Teves</name>
      </names>
  }

  case object FastOfEster extends NonTishaBeAvFast {
    override def date(year: Year): Day = Purim.date(year)-1

    final override protected def correctDate(result: Day): Day =
      // If on Friday or Saturday - move to Thursday
      if (result.isShabbos) result-2 else
      if (result.next.isShabbos) result-1 else
        result

    protected override val namesXml: Elem =
      <names>
        <name lang="en">Fast of Ester</name>
      </names>
  }

  sealed abstract class SpecialParsha extends Date with LoadNames with LoadMaftir with LoadHaftarah

  case object ParshasShekalim extends SpecialParsha {
    override def date(year: Year): Day = {
      val result = Purim.date(year).month.firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Parshas Shekalim</name>
      </names>

    protected override val maftirXml: Elem =
        <maftir book="Exodus" fromChapter="30" fromVerse="11" toVerse="16"/>

    protected override val haftarahXml: Elem =
      <haftarah book="II Kings">
        <custom n="Ashkenaz, Italki, Teiman, Chabad" fromChapter="12" fromVerse="1" toVerse="17"/>
        <custom n="Sefard" fromChapter="11" fromVerse="17" toChapter="12" toVerse="17"/>
      </haftarah>
  }

  case object ParshasZachor extends SpecialParsha {
    override def date(year: Year): Day = shabbosBefore(Purim.date(year))

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Parshas Zachor</name>
      </names>

    protected override val maftirXml: Elem =
        <maftir book="Deuteronomy" fromChapter="25" fromVerse="17" toVerse="19"/>

    protected override val haftarahXml: Elem =
      <haftarah book="I Samuel">
        <custom n="Ashkenaz, Chabad" fromChapter="15" fromVerse="2" toVerse="34"/>
        <custom n="Sefard" fromChapter="15" fromVerse="1" toVerse="34"/>
        <custom n="Teiman" fromChapter="14" fromVerse="52" toChapter="15" toVerse="33"/>
      </haftarah>
  }

  case object ParshasParah extends SpecialParsha {
    override def date(year: Year): Day = shabbosBefore(ParshasHachodesh.date(year))

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Parshas Parah</name>
      </names>

    protected override val maftirXml: Elem =
        <maftir book="Numbers" fromChapter="19" fromVerse="1" toVerse="22"/>

    protected override val haftarahXml: Elem =
      <haftarah book="Ezekiel" fromChapter="36" fromVerse="16">
        <custom n="Ashkenaz, Italki" toVerse="38"/>
        <custom n="Sefard" toVerse="36"/>
      </haftarah>
  }

  case object ParshasHachodesh extends SpecialParsha {
    override def date(year: Year): Day = {
      val result = year.month(Nisan).firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Parshas Hachodesh</name>
      </names>

    protected override val maftirXml: Elem =
        <maftir book="Exodus" fromChapter="12" fromVerse="1" toVerse="20"/>

    protected override val haftarahXml: Elem =
      <haftarah book="Ezekiel" fromChapter="45" toChapter="46">
        <custom n="Ashkenaz" fromVerse="16" toVerse="18"/>
        <custom n="Italki" fromVerse="18" toVerse="11"/>
        <custom n="Sefard" fromVerse="18" toVerse="15"/>
        <custom n="Teiman" fromVerse="9" toVerse="11"/>
      </haftarah>
  }

  case object ShabbosHagodol extends LoadNames with Date with LoadHaftarah {
    override def date(year: Year): Day = shabbosBefore(Pesach.date(year))

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Shabbos Hagodol</name>
      </names>

    // TODO Rule: Chabad only erev Pesah
    protected override val haftarahXml: Elem =
        <haftarah book="Malachi" fromChapter="3" fromVerse="4" toVerse="24"/>
  }

  case object Purim extends LoadNames with RabbinicFestival with VerySimpleReading with NoShabbosAllowed with LoadAliyot {
    override def date(year: Year): Day = year.latestAdar.day(14)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Purim</name>
      </names>

    protected override val torahXml: Elem =
      <torah book="Exodus" fromChapter="17" fromVerse="8" toVerse="16">
        <aliyah n="2" fromVerse="11"/>
        <aliyah n="3" fromVerse="14"/>
      </torah>
  }

  case object ShushanPurim extends RabbinicFestival with LoadNames with Date with NoShabbosAllowed with SimpleReading {
    override def date(year: Year): Day = Purim.date(year) + 1
    override def getReading(isShabbos: Boolean): Reading = Purim.getReading(isShabbos)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Shushan Purim</name>
      </names>
  }

  case object Pesach extends FestivalFirstDay with FirstDayOf with LoadAliyotForShabbosAndWeekday {
    final def date(year: Year): Day = year.month(Nisan).day(15)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Pesach</name>
      </names>

    protected override val torahXml: Elem =
      <torah book="Exodus" fromChapter="12" fromVerse="21" toVerse="51">
        <aliyah n="2" fromVerse="25"/>
        <aliyah n="3" fromVerse="29"/>
        <aliyah n="4" fromVerse="33" shabbos="true"/>
        <aliyah n="5" fromVerse="37"/>
        <aliyah n="6" fromVerse="43"/>
        <aliyah n="7" fromVerse="48" shabbos="true"/>
      </torah>

    protected override val maftirXml: Elem =
        <maftir book="Numbers" fromChapter="28" fromVerse="16" toVerse="25"/>

    protected override val haftarahXml: Elem =
      <haftarah book="Joshua">
        <custom n="Ashkenaz, Chabad">
          <part n="1" fromChapter="3" fromVerse="5" toVerse="7"/>
          <part n="2" fromChapter="5" fromVerse="2" toChapter="6" toVerse="1"/>
          <part n="3" fromChapter="6" fromVerse="27"/>
        </custom>
        <custom n="Sefard">
          <part n="1" fromChapter="5" fromVerse="2" toChapter="6" toVerse="1"/>
          <part n="2" fromChapter="6" fromVerse="27"/>
        </custom>
        <custom n="Frankfurt, Hagra" fromChapter="5" fromVerse="2" toChapter="6" toVerse="1"/>
      </haftarah>
  }

  case object Pesach2 extends FestivalSecondDay with NonFirstDayOf with NoShabbosAllowed with LoadAliyot {
    override def names: Names = ???
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 2
    override def secondDayOf: FestivalFirstDay = Pesach

    protected override val torahXml: Elem =
      <torah book="Leviticus" fromChapter="22" fromVerse="26" toChapter="23" toVerse="44">
        <aliyah n="1" fromChapter="22" fromVerse="26"/>
        <aliyah n="2" fromChapter="23" fromVerse="4"/>
        <aliyah n="3" fromChapter="23" fromVerse="15"/>
        <aliyah n="4" fromChapter="23" fromVerse="23"/>
        <aliyah n="5" fromChapter="23" fromVerse="33"/>
      </torah>

    protected override val haftarahXml: Elem =
      <haftarah book="II Kings">
        <custom n="Common">
          <part n="1" fromChapter="23" fromVerse="1" toVerse="9"/>
          <part n="2" fromChapter="23" fromVerse="21" toVerse="25"/>
        </custom>
        <custom n="Italki" fromChapter="23" fromVerse="21" toVerse="30"/>
        <custom n="Teiman">
          <part n="1" fromChapter="22" fromVerse="1" toVerse="7"/>
          <part n="2" fromChapter="23" fromVerse="21" toVerse="25"/>
        </custom>
      </haftarah>
  }

  sealed class PesachIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand)
  {
    override def names: Names = ???

    override def firstDay: Date = Pesach

    final override def getReading(
      isShabbos: Boolean,
      weeklyReading: Option[WeeklyReading],
      isPesachOnChamishi: Boolean
    ): Reading = {
      if (isShabbos) {
        require((dayNumber != 2) && (dayNumber != 6))
        Reading(
          aliyot = shabbosTorah,
          maftir = Pesach7.maftir,
          haftarah = Some(shabbosHaftarah)
        )
      } else {
        val aliyot: Torah = if (dayNumber == 2) {
          val all = Pesach2.aliyot(isShabbos)
          Seq(all(1), all(2), all(3) + all(4) + all(5))
        } else {
          val torah: Torah = dayNumber match {
            case 3 => day3
            case 4 => if (!isPesachOnChamishi) day4 else day3
            case 5 => if (!isPesachOnChamishi) day5 else day4
            case 6 => day6
          }

          torah :+ Pesach7.maftir.get
        }

        Reading(aliyot = aliyot, maftir = None, haftarah = None)
      }
    }

    private val day3: Torah = parseTorah(
      <torah book="Exodus" fromChapter="13" fromVerse="1" toVerse="16">
        <aliyah n="2" fromVerse="5"/>
        <aliyah n="3" fromVerse="11"/>
      </torah>)

    private val day4: Torah = parseTorah(
      <torah book="Exodus" fromChapter="22" fromVerse="24" toChapter="23" toVerse="19">
        <aliyah n="2" fromChapter="22" fromVerse="27"/>
        <aliyah n="3" fromChapter="23" fromVerse="6"/>
      </torah>)

    private val day5: Torah = parseTorah(
      <torah book="Exodus" fromChapter="34" fromVerse="1" toVerse="26">
        <aliyah n="2" fromChapter="34" fromVerse="4"/>
        <aliyah n="3" fromChapter="34" fromVerse="18"/>
      </torah>)

    private val day6: Torah = parseTorah(
      <torah book="Numbers" fromChapter="9" fromVerse="1" toVerse="14">
        <aliyah n="2" fromVerse="7"/>
        <aliyah n="3" fromVerse="9"/>
      </torah>)

    private val shabbosHaftarah: Haftarah.OptionalCustoms = parseHaftarah(
      <haftarah book="Ezekiel">
        <custom n="Common" fromChapter="37" fromVerse="1" toVerse="14"/>
        <custom n="Teiman" fromChapter="36" fromVerse="37" toChapter="37" toVerse="14"/>
      </haftarah>)
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

  case object Pesach7 extends FestivalFirstDay with NonFirstDayOf with LoadAliyotForShabbosAndWeekday {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 7

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Pesach 7</name>
      </names>

    protected override val torahXml: Elem =
      <torah book="Exodus" fromChapter="13" fromVerse="17" toChapter="15" toVerse="26">
        <aliyah n="2" fromChapter="13" fromVerse="20" shabbos="true"/>
        <aliyah n="3" fromChapter="14" fromVerse="1"/>
        <aliyah n="4" fromChapter="14" fromVerse="5" shabbos="true"/>
        <aliyah n="5" fromChapter="14" fromVerse="9"/>
        <aliyah n="6" fromChapter="14" fromVerse="15"/>
        <aliyah n="7" fromChapter="14" fromVerse="26"/>
      </torah>

    protected override val maftirXml: Elem =
        <maftir book="Numbers" fromChapter="28" fromVerse="19" toVerse="25"/>

    protected override val haftarahXml: Elem =
        <haftarah book="II Samuel" fromChapter="22" fromVerse="1" toVerse="51"/>
  }

  case object Pesach8 extends FestivalSecondDay with NonFirstDayOf with LoadNames {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 8
    override def secondDayOf: FestivalFirstDay = Pesach7

    override def aliyot(isShabbos: Boolean): Torah =
      if (!isShabbos) SheminiAtzeres.shortAliyot else SheminiAtzeres.aliyot(isShabbos)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Pesach 8</name>
      </names>

    protected override val haftarahXml: Elem =
        <haftarah book="Isaiah" fromChapter="10" fromVerse="32" toChapter="12" toVerse="6"/>
  }

  case object LagBaOmer extends LoadNames with Date {
    override def date(year: Year): Day = Pesach.date(year) + 33 // year.month(Iyar).day(18)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Lag Ba Omer</name>
      </names>
  }

  case object Shavuos extends FestivalFirstDay with FirstDayOf with NoShabbosAllowed with LoadAliyot {
    override def date(year: Year): Day = Pesach.date(year) + 50 // year.month(Sivan).day(6)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Shavuos</name>
      </names>

    protected override val torahXml: Elem =
      <torah book="Exodus" fromChapter="19" fromVerse="1" toChapter="20" toVerse="22">
        <aliyah n="2" fromChapter="19" fromVerse="7"/>
        <aliyah n="3" fromChapter="19" fromVerse="14"/>
        <aliyah n="4" fromChapter="19" fromVerse="20"/>
        <aliyah n="5" fromChapter="20" fromVerse="15"/>
      </torah>

    protected override val maftirXml: Elem =
        <maftir book="Numbers" fromChapter="28" fromVerse="26" toVerse="31"/>

    protected override val haftarahXml: Elem =
      <haftarah book="Ezekiel">
        <custom n="Common">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="28"/>
          <part n="2" fromChapter="3" fromVerse="12"/>
        </custom>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toChapter="2" toVerse="2"/>
          <part n="2" fromChapter="3" fromVerse="12"/>
        </custom>
      </haftarah>
  }

  case object Shavuos2 extends FestivalSecondDay with NonFirstDayOf {
    override def names: Names = ???
    override def firstDay: Date = Shavuos
    override def dayNumber: Int = 2
    override def secondDayOf: FestivalFirstDay = Shavuos

    override def aliyot(isShabbos: Boolean): Torah =
      (if (!isShabbos) Pesach8 else SheminiAtzeres).aliyot(isShabbos)

    protected override val haftarahXml: Elem =
      <haftarah book="Habakkuk">
        <custom n="Ashkenaz" fromChapter="3" fromVerse="1" toVerse="19"/>
        <custom n="Sefard" fromChapter="2" fromVerse="20" toChapter="3" toVerse="19"/>
      </haftarah>
  }

  case object FastOfTammuz extends NonTishaBeAvFast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tammuz).day(17)

    protected override val namesXml: Elem =
      <names>
        <name lang="en">Fast of Tammuz</name>
      </names>
  }

  case object TishaBeAv extends Fast with PostponeOnShabbos with LoadAliyot with LoadHaftarah {
    override def date(year: Year): Day = year.month(Av).day(9)
    override def getReading(isShabbos: Boolean): Reading = readingWithLastAliyaAsMaftir(aliyot(isShabbos), haftarah.get)

    protected override val namesXml: Elem =
      <names>
        <name lang="en" transliterated="yes">Tisha BeAv</name>
      </names>

    protected override val torahXml: Elem =
      <torah book="Deuteronomy" fromChapter="4" fromVerse="25" toVerse="40">
        <aliyah n="2" fromVerse="30"/>
        <aliyah n="3" fromVerse="36"/>
      </torah>

    protected override val haftarahXml: Elem =
      <haftarah book="Jeremiah">
        <custom n="Common" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
        <custom n="Teiman">
          <part n="1" fromChapter="6" fromVerse="16" toVerse="17"/>
          <part n="2" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
        </custom>
      </haftarah>
  }

  private def readingWithLastAliyaAsMaftir(aliyot: Torah, haftarah: Haftarah.OptionalCustoms): Reading = Reading(
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
    Purim, ShushanPurim
  )

  val fasts: Set[Fast] = Set(FastOfGedalia, FastOfTeves, FastOfEster, FastOfTammuz, TishaBeAv)

  val daysWithSpecialReadingsNotFestivals: Set[Date with WithReading] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[WithReading] =
    Set.empty[WithReading] ++ festivals(inHolyLand) ++ fasts ++ rabbinicFestivals

  // TODO list all days with haftarah - but why?

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)

  private def parseTorahForShabbosAndWeekday(element: Elem): (Torah, Torah) = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: ChumashSpan.BookSpan = ChumashSpan.parse(attributes).resolve
    attributes.close()

    val fromChapter: Int = bookSpan.span.from.chapter
    def parseNumberedWithShabbos(attributes: Attributes): (Span.Numbered, Boolean) =
      (parseNumbered(fromChapter)(attributes), attributes.doGetBoolean("shabbos"))

    val result: Seq[(Span.Numbered, Boolean)] =
      elements.map(element => XML.parseEmpty(element, "aliyah", parseNumberedWithShabbos))
    val shabbosAliyot: Torah = Aliyot(bookSpan, result.map(_._1), number = Some(7))
    val toDrop: Set[Int] = result.filter(_._2).map(_._1.n).toSet
    require(!toDrop.contains(1))
    val weekdayAliyot =
      shabbosAliyot.zipWithIndex.filterNot { case (_, index: Int) => toDrop.contains(index + 1) }.map(_._1)
    (shabbosAliyot, weekdayAliyot)
  }

  private def parseTorah(element: Elem): Torah = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: ChumashSpan.BookSpan = ChumashSpan.parse(attributes).resolve
    attributes.close()
    if (elements.isEmpty) Aliyot(bookSpan, Seq(bookSpan.span)) else parseAliyot(bookSpan, elements)
  }

  private def parseAliyot(bookSpan: ChumashSpan.BookSpan, elements: Seq[Elem]): Aliyot.Torah = {
    val fromChapter: Int = bookSpan.span.from.chapter
    val result = elements.map(element => XML.parseEmpty(element, "aliyah", parseNumbered(fromChapter)))
    Aliyot(bookSpan, result, number = None)
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

  private def parseHaftarah(element: Elem): Haftarah.OptionalCustoms = Haftarah.parse(element, full= true)

  private def parseHaftarahNotFull(element: Elem): Haftarah.OptionalCustoms = Haftarah.parse(element, full= false)
}
