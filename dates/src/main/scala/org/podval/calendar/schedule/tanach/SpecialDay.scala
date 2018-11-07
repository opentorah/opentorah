package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.{Attributes, Names, WithNames, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Aliyot, Custom, Span}
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.Jewish.Month.Name._
import org.podval.calendar.jewish.{JewishDay, JewishYear}

import scala.xml.Elem

// TODO verify Torah for Shemini Atzeres, Pesach8 and Shavuos 2!
// TODO number of aliyot: Shabbos and Simchas Torah - 7; Yom Kippur - 6 (even on Shabbos); Yom Tov - 5;
// Intermediate and Rosh Chodesh - 4; Chanukkah, Purim, Fast, Sheni/Chamishi and Shabbos afternoon - 3.

object SpecialDay {

  type Torah = Seq[ChumashSpan.BookSpan]

  sealed trait Date extends WithNames {
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

  sealed trait WeekdayReading {
    def getWeekdayReading: Reading
  }

  sealed trait WeekdayReadingOnly extends WeekdayReading {
    final override def getWeekdayReading: Reading = Reading(torah, maftir, haftarah)

    protected def torah: Torah

    protected def maftir: ChumashSpan.BookSpan

    protected def haftarah: Haftarah.Customs
  }

  sealed trait ShabbosReading {
    def getShabbosReading: Reading
  }

  sealed trait ShabbosAndWeekdayReading extends ShabbosReading with WeekdayReading {
    final override def getWeekdayReading: Reading = getReading(weekdayTorah)

    final override def getShabbosReading: Reading = getReading(shabbosTorah)

    private def getReading(torah: Torah): Reading = Reading(torah, maftir, haftarah)

    protected val torahXml: Elem

    lazy final val (shabbosTorah, weekdayTorah) = parseTorahForShabbosAndWeekday(torahXml)

    protected def maftir: ChumashSpan.BookSpan

    protected def haftarah: Haftarah.Customs
  }

  sealed trait AfternoonReading {
    def getAfternoonReading: Reading
  }

  sealed trait FestivalOrIntermediate extends Date

  sealed trait Festival extends FestivalOrIntermediate with WeekdayReading

  private object Intermediate {
    final val shabbosTorah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="33" fromVerse="12" toChapter="34" toVerse="26">
        <aliyah n="2" fromChapter="33" fromVerse="17"/>
        <aliyah n="3" fromChapter="33" fromVerse="20"/>
        <aliyah n="4" fromChapter="34" fromVerse="1"/>
        <aliyah n="5" fromChapter="34" fromVerse="4"/>
        <aliyah n="6" fromChapter="34" fromVerse="11"/>
        <aliyah n="7" fromChapter="34" fromVerse="18"/>
      </torah>)
  }

  sealed abstract class Intermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends FestivalOrIntermediate with NonFirstDayOf with ShabbosReading
  {
    final override def dayNumber: Int = intermediateDayNumber + (if (inHolyLand) 1 else 2)

    final override def getShabbosReading: Reading = Reading(
      torah = Intermediate.shabbosTorah,
      maftir = shabbosMaftir,
      haftarah = shabbosHaftarah
    )

    protected def shabbosMaftir: ChumashSpan.BookSpan

    protected def shabbosHaftarah: Haftarah.Customs
  }

  sealed trait RabbinicFestival extends Date

  /*
     TODO add special parshios, Shabbos Hagodol, erev rosh chodesh...
     Shabbos  Rosh Chodesh Additional Haftorah

     on Shabos rosh hodesh Menachem Ov  - only maftir rosh chodesh, haftarah - current and the added piece;
     on Shabbos if Sunday is also Rosh Chodesh: add "Shabbos Erev Rosh Chodesh Additional Haftorah".

     If Rosh Chodesh Elul is Shabbos, on 14th something merges...

   TODO RULE: When another haftarah (e.g., Shekalim) takes over, add "Shabbos [Erev] Rosh Chodesh Additional Haftorah"
             Chabad: always
             Fes: only EREV rosh chodesh
             остальные не добавляют

Какой-то шабат выпал на канун рош-ходеша.
Широкий обычай - читают афтару кануна рош ходеша.
Обычай Феса - читают афтару недельной главы и добавляют эту добавку - первый и последний стих из афтары кануна рош-ходеша.
Для широкого обычая есть ситуации, когда афтара недельной главы отодвигаются по другой причине.
Это не только канун рош ходеша, но и рош ходеш или суббота перед рош ходеш адар (Шкалим) или суббота перед рош-ходеш нисан (Аходеш).
В этом случае  в Хабаде читают афтару этого более сильного случая (рош ходеша, Шкалим или Аходеш) и добавляют эту добавку.
Особый случай рош ходеш менахем ав или его канун и канун рош ходеш элул.
В этом случае читают афтару недельной главы (которая на самом деле из трех увещеваний или семи утешений),
а в конце добавляют эти два стиха из афтары кануна рош ходеша.
  */
  case object RoshChodesh extends WithNames {
    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Rosh Chodesh</name>
      </names>)

    private val torah: Torah = parseTorah(
      <torah book="Numbers" fromChapter="28" fromVerse="1" toVerse="15">
        <aliyah n="2" fromVerse="3"/>
        <aliyah n="3" fromVerse="4"/>
        <aliyah n="4" fromVerse="6"/>
        <aliyah n="5" fromVerse="9"/>
        <aliyah n="6" fromVerse="11"/>
      </torah>)

    def weekday: Reading = Reading(
      Seq(torah(0)+torah(1), torah(1)+torah(2), torah(3)+torah(4), torah(5)),
    )

    def in3aliyot: Torah = Seq(torah(0)+torah(1)+torah(2), torah(3)+torah(4), torah(5))

    val shabbosMaftir: ChumashSpan.BookSpan = torah(4)+torah(5)

    val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="66">
        <part n="1" fromVerse="1" toVerse="24"/>
        <part n="2" fromVerse="23" toVerse="23"/>
      </haftarah>)

    // TODO added by Chabad only
    private val shabbosAdditionalHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="66">
        <part n="1" fromVerse="1" toVerse="1"/>
        <part n="2" fromVerse="23" toVerse="24"/>
        <part n="3" fromVerse="23" toVerse="23"/>
      </haftarah>)

    private val shabbosErevRoshChodeshHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel" fromChapter="20" fromVerse="18" toVerse="42"/>)

    // TODO added by Chabad and Fes only; after haftarah if it wasn't pushed off by the haftarah Shabbos erev Rosh Chodesh.
    private val shabbosErevRoshChodeshAdditionalHaftorah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel" fromChapter="20">
        <part n="1" fromVerse="18" toVerse="18"/>
        <part n="2" fromVerse="42" toVerse="42"/>
      </haftarah>)
  }

  private object Fast {
    final val afternoonTorah: Torah = parseTorah(
        <torah book="Exodus" fromChapter="32" fromVerse="11" toVerse="14"/>) ++ parseTorah(
      <torah book="Exodus" fromChapter="34" fromVerse="1" toVerse="10">
        <aliyah n="2" fromVerse="4"/>
      </torah>)

    def afternoonHaftarah(exceptions: Option[Haftarah.OptionalCustoms]): Haftarah.OptionalCustoms =
      exceptions.fold(defaultAfternoonHaftarah) { afternoonHaftarahExceptions =>
        defaultAfternoonHaftarah ++ afternoonHaftarahExceptions }

    private val defaultAfternoonHaftarah: Haftarah.OptionalCustoms = Haftarah.parseOptional(full = true, element =
      <haftarah>
        <custom n="Common" empty="true"/>
        <custom n="Ashkenaz, Chabad, Morocco" book="Isaiah" fromChapter="55" fromVerse="6" toChapter="56" toVerse="8"/>
        <custom n="Algeria">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>)
  }

  sealed trait Fast extends Date with WeekdayReading with AfternoonReading {
    final override def getAfternoonReading: Reading = Reading.mkOptional(
      torah = Fast.afternoonTorah,
      haftarah = Fast.afternoonHaftarah(afternoonHaftarahExceptions)
    )

    protected val afternoonHaftarahExceptions: Option[Haftarah.OptionalCustoms] = None
  }

  sealed trait NonTishaBeAvFast extends Fast {
    final override def getWeekdayReading: Reading = Reading(Fast.afternoonTorah)
  }

  case object RoshHashanah1 extends Festival with ShabbosAndWeekdayReading {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(1)

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Rosh Hashanah</name>
      </names>)

    protected override val torahXml: Elem =
      <torah book="Genesis" fromChapter="21" fromVerse="1" toVerse="34">
        <aliyah n="2" fromVerse="5"/>
        <aliyah n="3" fromVerse="9" shabbos="true"/>
        <aliyah n="4" fromVerse="13"/>
        <aliyah n="5" fromVerse="18" shabbos="true"/>
        <aliyah n="6" fromVerse="22"/>
        <aliyah n="7" fromVerse="28"/>
      </torah>

    override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Numbers" fromChapter="29" fromVerse="1" toVerse="6"/>)

    override val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="I Samuel" fromChapter="1" fromVerse="1" toChapter="2" toVerse="10"/>)
  }

  case object RoshHashanah2 extends Festival with NonFirstDayOf with WeekdayReadingOnly {
    override def firstDay: Date = RoshHashanah1

    override def dayNumber: Int = 2

    override val names: Names = namesWithNumber(RoshHashanah1, 2)

    protected override val torah: Torah = parseTorah(
      <torah book="Genesis" fromChapter="22" fromVerse="1" toVerse="24">
        <aliyah n="2" fromVerse="4"/>
        <aliyah n="3" fromVerse="9"/>
        <aliyah n="4" fromVerse="15"/>
        <aliyah n="5" fromVerse="20"/>
      </torah>)

    protected override def maftir: ChumashSpan.BookSpan = RoshHashanah1.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Jeremiah" fromChapter="31" fromVerse="1" toVerse="19"/>)
  }

  case object FastOfGedalia extends NonTishaBeAvFast with PostponeOnShabbos {
    final override def date(year: Year): Day = year.month(Tishrei).day(3)

    override val names: Names = parseNames(
      <names>
        <name lang="en">Fast of Gedalia</name>
      </names>)

    protected override val afternoonHaftarahExceptions: Option[Haftarah.OptionalCustoms] = Some(parseHaftarahNotFull(
      <haftarah>
        <custom n="Morocco">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>))
  }

  case object YomKippur extends Festival with ShabbosAndWeekdayReading with AfternoonReading {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(10)

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Yom Kippur</name>
      </names>)

    protected override val torahXml: Elem =
      <torah book="Leviticus" fromChapter="16" fromVerse="1" toVerse="34">
        <aliyah n="2" fromVerse="4" shabbos="true"/>
        <aliyah n="3" fromVerse="7"/>
        <aliyah n="4" fromVerse="12"/>
        <aliyah n="5" fromVerse="18"/>
        <aliyah n="6" fromVerse="25"/>
        <aliyah n="7" fromVerse="31"/>
      </torah>

    protected override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Numbers" fromChapter="29" fromVerse="7" toVerse="11"/>)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah">
        <custom n="Common" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
        <custom n="Italki, Teiman">
          <part n="1" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
          <part n="2" fromChapter="59" fromVerse="20" toVerse="21"/>
        </custom>
      </haftarah>)

    override def getAfternoonReading: Reading = Reading(afternoonAliyot, afternoonHaftarah)

    private val afternoonAliyot: Torah = parseTorah(
      <torah book="Leviticus" fromChapter="18" fromVerse="1" toVerse="30">
        <aliyah n="2" fromVerse="6"/>
        <aliyah n="3" fromVerse="22"/>
      </torah>)

    private val afternoonHaftarah: Haftarah.Customs = parseHaftarah(
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
      </haftarah>)
  }

  case object Succos extends Festival with FirstDayOf with ShabbosAndWeekdayReading {
    override def date(year: JewishYear): JewishDay = year.month(Tishrei).day(15)

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Succos</name>
      </names>)

    protected override val torahXml: Elem =
      <torah book="Leviticus" fromChapter="22" fromVerse="26" toChapter="23" toVerse="44">
        <aliyah n="2" fromChapter="23" fromVerse="1" shabbos="true"/>
        <aliyah n="3" fromChapter="23" fromVerse="4"/>
        <aliyah n="4" fromChapter="23" fromVerse="9" shabbos="true"/>
        <aliyah n="5" fromChapter="23" fromVerse="15"/>
        <aliyah n="6" fromChapter="23" fromVerse="23"/>
        <aliyah n="7" fromChapter="23" fromVerse="33"/>
      </torah>

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Zechariah" toChapter="14" toVerse="21">
        <custom n="Common" fromChapter="14" fromVerse="1"/>
        <custom n="Teiman" fromChapter="13" fromVerse="9"/>
      </haftarah>)

    override def maftir: ChumashSpan.BookSpan = korbanot.head

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

  case object Succos2 extends Festival with NonFirstDayOf with ShabbosReading {
    override def names: Names = namesWithNumber(Succos, 2)

    override def firstDay: Date = Succos

    override def dayNumber: Int = 2

    override def getWeekdayReading: Reading = getReading(Succos.weekdayTorah)
    override def getShabbosReading: Reading = getReading(Succos.shabbosTorah)
    private def getReading(torah: Torah): Reading = Reading(torah, maftir, haftarah)

    private val maftir: ChumashSpan.BookSpan = Succos.maftir

    private val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings">
        <custom n="Common" fromChapter="8" fromVerse="2" toVerse="21"/>
        <custom n="Italki" fromChapter="7" fromVerse="51" toChapter="8" toVerse="15"/>
        <custom n="Teiman" fromChapter="7" fromVerse="51" toChapter="8" toVerse="21"/>
      </haftarah>)
  }

  private object SuccosIntermediate extends WithNames {
    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Succos Intermediate</name>
      </names>)

    val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="38">
        <custom n="Common" fromVerse="18" toChapter="39" toVerse="16"/>
        <custom n="Italki, Teiman" fromVerse="1" toChapter="38" toVerse="23"/>
      </haftarah>)
  }

  sealed class SuccosIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand) with WeekdayReading with ShabbosReading
  {
    final override val names: Names = namesWithNumber(SuccosIntermediate, intermediateDayNumber)

    override def firstDay: Date = Succos

    override def getWeekdayReading: Reading = {
      if (intermediateDayNumber == 6) require(inHolyLand)

      val n: Int = Math.min(dayNumber, 5)

      val torah: Custom.Of[Torah] = Map(
        Custom.Common -> (Seq(korbanot(n), korbanot(n+1), korbanot(n+2)) :+ shabbosMaftir),
        Custom.RavNaeHolyLand -> Seq(korbanot(n), korbanot(n), korbanot(n), korbanot(n))
      )

      Reading(torah)
    }

    private def korbanot(n: Int): ChumashSpan.BookSpan = Succos.korbanot(n-1)

    protected override def shabbosMaftir: ChumashSpan.BookSpan =
      if (inHolyLand) korbanot(dayNumber) else korbanot(dayNumber-1) + korbanot(dayNumber)

    protected override def shabbosHaftarah: Haftarah.Customs = SuccosIntermediate.shabbosHaftarah
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

  case object SheminiAtzeres extends Festival with NonFirstDayOf with ShabbosAndWeekdayReading {
    override def firstDay: Date = Succos
    override def dayNumber: Int = 8

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Shemini Atzeres</name>
      </names>)

    protected override val torahXml: Elem =
      <torah book="Deuteronomy" fromChapter="14" fromVerse="22" toChapter="16" toVerse="17">
        <aliyah n="2" fromChapter="15" fromVerse="1" shabbos="true"/>
        <aliyah n="3" fromChapter="15" fromVerse="19" shabbos="true"/>
        <aliyah n="4" fromChapter="16" fromVerse="1"/>
        <aliyah n="5" fromChapter="16" fromVerse="4"/>
        <aliyah n="6" fromChapter="16" fromVerse="9"/>
        <aliyah n="7" fromChapter="16" fromVerse="13"/>
      </torah>

    def shortTorah: Torah = shabbosTorah.drop(2)

    override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Numbers" fromChapter="29" fromVerse="35" toChapter="30" toVerse="1"/>)

    /*
  Artscroll gives custom Ashkenaz ending at 9:1,
  but @michaelko58 insists that it is the same as Sefard and ends at 8:66.
  His explanation: "there are some ashkenazic communities that follow custom Italki.
  It is possible that this is a difference between chassidim and litaim."
  */
    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings" fromChapter="8" fromVerse="54">
        <custom n="Common" toChapter="8" toVerse="66"/>
        <custom n="Italki, Chabad" toChapter="9" toVerse="1"/>
      </haftarah>)
  }

  sealed trait SimchasTorahCommon extends Festival with NonFirstDayOf with WeekdayReadingOnly {
    final override def firstDay: Date = Succos

    // Zos Haberacha + Chassan Bereishis
    protected override val torah: Torah = parseTorah(
      <torah book="Deuteronomy" fromChapter="33" fromVerse="1" toChapter="34" toVerse="12">
        <aliyah n="2" fromChapter="33" fromVerse="5"/>
        <aliyah n="3" fromChapter="33" fromVerse="13"/>
        <aliyah n="4" fromChapter="33" fromVerse="18"/>
        <aliyah n="5" fromChapter="33" fromVerse="22"/>
        <aliyah n="6" fromChapter="33" fromVerse="27"/>
      </torah>) ++ parseTorah(
        <torah book="Genesis" fromChapter="1" fromVerse="1" toChapter="2" toVerse="3"/>)

    protected override val maftir: ChumashSpan.BookSpan = SheminiAtzeres.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Joshua">
        <custom n="Common, Italki" fromChapter="1" fromVerse="1" toVerse="18"/>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="9"/>
          <part n="2" fromChapter="6" fromVerse="27"/>
        </custom>
      </haftarah>)
  }

  case object SimchasTorah extends SimchasTorahCommon {
    override def dayNumber: Int = 9

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Simchas Torah</name>
      </names>)
  }

  case object SheminiAtzeresAndSimchasTorahInHolyLand extends SimchasTorahCommon with ShabbosReading {
    override def dayNumber: Int = 8

    override def getShabbosReading: Reading = getWeekdayReading

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Shemini Atzeres and Simchas Torah</name>
      </names>)
  }

  case object ShabbosBereishis extends Date {
    override def date(year: Year): Day = shabbosAfter(SimchasTorah.date(year))

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Shabbos Bereishis</name>
      </names>)
  }

  private object Chanukah extends WithNames {
    val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Chanukah</name>
      </names>)

    // last aliyah is for Zos Channukah
    val korbanot: Torah = parseTorah(
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

    val haftarahShabbos1: Haftarah.Customs = parseHaftarah(
        <haftarah book="Zechariah" fromChapter="2" fromVerse="14" toChapter="4" toVerse="7"/>)

    val haftarahShabbos2: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings" fromChapter="7" fromVerse="40" toChapter="7">
        <custom n="Common" toVerse="50"/>
        <custom n="Italki" toVerse="51"/>
      </haftarah>)
  }

  sealed class Chanukah(override val dayNumber: Int) extends WithNames with DayOf with RabbinicFestival {
    import Chanukah.korbanot

    final override val names: Names = namesWithNumber(Chanukah, dayNumber)

    final override def firstDay: Date = Chanukah1

    override def date(year: Year): Day = Chanukah1.date(year)+(dayNumber-1)

    final def getShabbosReading(weeklyReading: WeeklyReading, isRoshChodesh: Boolean): Reading = {
      if (!isRoshChodesh) {
        // Replace maftir and haftarah
        val shabbosMaftir: ChumashSpan.BookSpan = full(dayNumber)
        val shabbosHaftarah: Haftarah.Customs =
          if (dayNumber < 8) Chanukah.haftarahShabbos1 else Chanukah.haftarahShabbos2

        Reading(
          torah = weeklyReading.getReading.customs.mapValues(_.torah),
          maftir = shabbosMaftir,
          haftarah = shabbosHaftarah
        )
      } else {
        // TODO for all customs:
        //  val torah = weeklyReading...
        //  torah = torah.take(5) :+ (torah(5)+torah(6))
        //  maftir = Some(RoshChodesh.shabbosMaftir),
        //  add Shabbos Rosh Chodesh Haftara to the one there already
        ???
      }
    }

    final def getWeekdayReading(isRoshChodesh: Boolean): Reading = {
      val (common: Torah, ashkenazAndChabadTail: Torah, sefardTail: Torah) =
        if (dayNumber == 1) (
          Seq(korbanot.head),
          Seq(first(dayNumber), second(dayNumber)),
          Seq(full(dayNumber), full(dayNumber + 1))
        ) else (
          // TODO it seems that for 1 < dayNumber < 8, there is no difference between the customs?!
          Seq(first(dayNumber), second(dayNumber)),
          Seq(if (dayNumber == 8) korbanot.last else full(dayNumber)),
          Seq(if (dayNumber == 8) full(dayNumber) + korbanot.last else full(dayNumber))
        )

      val ashkenazAndChabad = common ++ ashkenazAndChabadTail
      val sefard = common ++ sefardTail
      require(ashkenazAndChabad.length == 3)
      require(sefard.length == 3)

      val torah: Custom.Of[Torah] = Map(
        Custom.Ashkenaz -> ashkenazAndChabad,
        Custom.Chabad -> ashkenazAndChabad,
        Custom.Sefard -> sefard
      )

      val torahResult: Custom.Of[Torah] = if (!isRoshChodesh) torah else {
        val roshChodesh = RoshChodesh.in3aliyot
        torah.mapValues(torah => roshChodesh :+ torah.last)
      }

      Reading(torahResult)
    }

    private def first(n: Int): ChumashSpan.BookSpan = korbanot(2*n-1)
    private def second(n: Int): ChumashSpan.BookSpan = korbanot(2*n  )
    private def full(n: Int): ChumashSpan.BookSpan = first(n)+second(n)
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
    override def date(year: Year): Day = year.month(Teves).day(10)

    override val names: Names = parseNames(
      <names>
        <name lang="en">Fast of 10th of Teves</name>
      </names>)
  }

  case object FastOfEster extends NonTishaBeAvFast {
    override def date(year: Year): Day = Purim.date(year)-1

    protected override def correctDate(result: Day): Day =
      // If on Friday or Saturday - move to Thursday
      if (result.isShabbos) result-2 else
      if (result.next.isShabbos) result-1 else
        result

    override val names: Names = parseNames(
      <names>
        <name lang="en">Fast of Ester</name>
      </names>)
  }

  sealed abstract class SpecialParsha extends Date {
    protected def maftir: ChumashSpan.BookSpan

    protected def haftarah: Haftarah.Customs
  }

  case object ParshasShekalim extends SpecialParsha {
    override def date(year: Year): Day = {
      val result = Purim.date(year).month.firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Parshas Shekalim</name>
      </names>)

    protected override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Exodus" fromChapter="30" fromVerse="11" toVerse="16"/>)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="II Kings">
        <custom n="Ashkenaz, Italki, Teiman, Chabad" fromChapter="12" fromVerse="1" toVerse="17"/>
        <custom n="Sefard" fromChapter="11" fromVerse="17" toChapter="12" toVerse="17"/>
      </haftarah>)
  }

  case object ParshasZachor extends SpecialParsha {
    override def date(year: Year): Day = shabbosBefore(Purim.date(year))

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Parshas Zachor</name>
      </names>)

    protected override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Deuteronomy" fromChapter="25" fromVerse="17" toVerse="19"/>)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel">
        <custom n="Ashkenaz, Chabad" fromChapter="15" fromVerse="2" toVerse="34"/>
        <custom n="Sefard" fromChapter="15" fromVerse="1" toVerse="34"/>
        <custom n="Teiman" fromChapter="14" fromVerse="52" toChapter="15" toVerse="33"/>
      </haftarah>)
  }

  case object ParshasParah extends SpecialParsha {
    override def date(year: Year): Day = shabbosBefore(ParshasHachodesh.date(year))

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Parshas Parah</name>
      </names>)

    protected override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Numbers" fromChapter="19" fromVerse="1" toVerse="22"/>)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="36" fromVerse="16">
        <custom n="Ashkenaz, Italki" toVerse="38"/>
        <custom n="Sefard" toVerse="36"/>
      </haftarah>)
  }

  case object ParshasHachodesh extends SpecialParsha {
    override def date(year: Year): Day = {
      val result = year.month(Nisan).firstDay
      if (result.isShabbos) result else shabbosBefore(result)
    }

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Parshas Hachodesh</name>
      </names>)

    protected override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Exodus" fromChapter="12" fromVerse="1" toVerse="20"/>)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="45" toChapter="46">
        <custom n="Ashkenaz" fromVerse="16" toVerse="18"/>
        <custom n="Italki" fromVerse="18" toVerse="11"/>
        <custom n="Sefard" fromVerse="18" toVerse="15"/>
        <custom n="Teiman" fromVerse="9" toVerse="11"/>
      </haftarah>)
  }

  case object ShabbosHagodol extends Date {
    override def date(year: Year): Day = shabbosBefore(Pesach.date(year))

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Shabbos Hagodol</name>
      </names>)

    // TODO Rule: Chabad only erev Pesah
    val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="Malachi" fromChapter="3" fromVerse="4" toVerse="24"/>)
  }

  private object PurimCommon {
    val torah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="17" fromVerse="8" toVerse="16">
        <aliyah n="2" fromVerse="11"/>
        <aliyah n="3" fromVerse="14"/>
      </torah>)
  }

  sealed trait PurimCommon extends RabbinicFestival with WeekdayReading {
    final override def getWeekdayReading: Reading = Reading(PurimCommon.torah)
  }

  case object Purim extends PurimCommon {
    override def date(year: Year): Day = year.latestAdar.day(14)

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Purim</name>
      </names>)

  }

  case object ShushanPurim extends PurimCommon {
    override def date(year: Year): Day = Purim.date(year) + 1

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Shushan Purim</name>
      </names>)
  }

  case object Pesach extends Festival with FirstDayOf with ShabbosAndWeekdayReading {
    final def date(year: Year): Day = year.month(Nisan).day(15)

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Pesach</name>
      </names>)

    protected override val torahXml: Elem =
      <torah book="Exodus" fromChapter="12" fromVerse="21" toVerse="51">
        <aliyah n="2" fromVerse="25"/>
        <aliyah n="3" fromVerse="29"/>
        <aliyah n="4" fromVerse="33" shabbos="true"/>
        <aliyah n="5" fromVerse="37"/>
        <aliyah n="6" fromVerse="43"/>
        <aliyah n="7" fromVerse="48" shabbos="true"/>
      </torah>

    override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Numbers" fromChapter="28" fromVerse="16" toVerse="25"/>)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
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
      </haftarah>)
  }

  case object Pesach2 extends Festival with NonFirstDayOf with WeekdayReadingOnly {
    override val names: Names = namesWithNumber(Pesach, 2)

    override def firstDay: Date = Pesach

    override def dayNumber: Int = 2

    override val torah: Torah = parseTorah(
      <torah book="Leviticus" fromChapter="22" fromVerse="26" toChapter="23" toVerse="44">
        <aliyah n="1" fromChapter="22" fromVerse="26"/>
        <aliyah n="2" fromChapter="23" fromVerse="4"/>
        <aliyah n="3" fromChapter="23" fromVerse="15"/>
        <aliyah n="4" fromChapter="23" fromVerse="23"/>
        <aliyah n="5" fromChapter="23" fromVerse="33"/>
      </torah>)

    protected override val maftir: ChumashSpan.BookSpan = Pesach.maftir

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
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
      </haftarah>)
  }

  private object PesachIntermediate extends WithNames {
    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Pesach Intermediate</name>
      </names>)

    def torah(dayNumber: Int, isPesachOnChamishi: Boolean): Torah = dayNumber match {
      case 3 => day3
      case 4 => if (!isPesachOnChamishi) day4 else day3
      case 5 => if (!isPesachOnChamishi) day5 else day4
      case 6 => day6
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

    val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel">
        <custom n="Common" fromChapter="37" fromVerse="1" toVerse="14"/>
        <custom n="Teiman" fromChapter="36" fromVerse="37" toChapter="37" toVerse="14"/>
      </haftarah>)
  }

  sealed class PesachIntermediate(intermediateDayNumber: Int, inHolyLand: Boolean)
    extends Intermediate(intermediateDayNumber, inHolyLand)
  {
    final override val names: Names = namesWithNumber(PesachIntermediate, intermediateDayNumber)

    final override def firstDay: Date = Pesach

    protected override def shabbosMaftir: ChumashSpan.BookSpan = Pesach7.maftir

    protected override def shabbosHaftarah: Haftarah.Customs = PesachIntermediate.shabbosHaftarah

    final def getWeekdayReading(isPesachOnChamishi: Boolean): Reading = {
      val torah: Torah = if (dayNumber == 2) {
        val all = Pesach2.torah
        Seq(all(1), all(2), all(3) + all(4) + all(5))
      } else {
        PesachIntermediate.torah(dayNumber, isPesachOnChamishi) :+ shabbosMaftir
      }

      Reading(torah)
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

  case object Pesach7 extends Festival with NonFirstDayOf with ShabbosAndWeekdayReading {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 7

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Pesach 7</name>
      </names>)

    protected override val torahXml: Elem =
      <torah book="Exodus" fromChapter="13" fromVerse="17" toChapter="15" toVerse="26">
        <aliyah n="2" fromChapter="13" fromVerse="20" shabbos="true"/>
        <aliyah n="3" fromChapter="14" fromVerse="1"/>
        <aliyah n="4" fromChapter="14" fromVerse="5" shabbos="true"/>
        <aliyah n="5" fromChapter="14" fromVerse="9"/>
        <aliyah n="6" fromChapter="14" fromVerse="15"/>
        <aliyah n="7" fromChapter="14" fromVerse="26"/>
      </torah>

    override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Numbers" fromChapter="28" fromVerse="19" toVerse="25"/>)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="II Samuel" fromChapter="22" fromVerse="1" toVerse="51"/>)
  }

  case object Pesach8 extends Festival with NonFirstDayOf with ShabbosReading {
    override def firstDay: Date = Pesach
    override def dayNumber: Int = 8

    override def getWeekdayReading: Reading = getReading(SheminiAtzeres.shortTorah)
    override def getShabbosReading: Reading = getReading(SheminiAtzeres.shabbosTorah)
    private def getReading(torah: Torah): Reading = Reading(torah, maftir, haftarah)

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Pesach 8</name>
      </names>)

    private val maftir: ChumashSpan.BookSpan = Pesach7.maftir

    private val haftarah: Haftarah.Customs = parseHaftarah(
        <haftarah book="Isaiah" fromChapter="10" fromVerse="32" toChapter="12" toVerse="6"/>)
  }

  case object LagBaOmer extends Date {
    override def date(year: Year): Day = Pesach.date(year) + 33 // year.month(Iyar).day(18)

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Lag Ba Omer</name>
      </names>)
  }

  case object Shavuos extends Festival with FirstDayOf with WeekdayReadingOnly {
    override def date(year: Year): Day = Pesach.date(year) + 50 // year.month(Sivan).day(6)

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Shavuos</name>
      </names>)

    protected override val torah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="19" fromVerse="1" toChapter="20" toVerse="22">
        <aliyah n="2" fromChapter="19" fromVerse="7"/>
        <aliyah n="3" fromChapter="19" fromVerse="14"/>
        <aliyah n="4" fromChapter="19" fromVerse="20"/>
        <aliyah n="5" fromChapter="20" fromVerse="15"/>
      </torah>)

    override val maftir: ChumashSpan.BookSpan = parseMaftir(
        <maftir book="Numbers" fromChapter="28" fromVerse="26" toVerse="31"/>)

    protected override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel">
        <custom n="Common">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="28"/>
          <part n="2" fromChapter="3" fromVerse="12"/>
        </custom>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toChapter="2" toVerse="2"/>
          <part n="2" fromChapter="3" fromVerse="12"/>
        </custom>
      </haftarah>)
  }

  case object Shavuos2 extends Festival with NonFirstDayOf with ShabbosReading  {
    override val names: Names = namesWithNumber(Shavuos, 2)

    override def firstDay: Date = Shavuos

    override def dayNumber: Int = 2

    override def getWeekdayReading: Reading = getReading(SheminiAtzeres.weekdayTorah)
    override def getShabbosReading: Reading = getReading(SheminiAtzeres.shabbosTorah)
    private def getReading(torah: Torah): Reading = Reading(torah, maftir, haftarah)

    private val maftir: ChumashSpan.BookSpan = Shavuos.maftir

    private val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Habakkuk">
        <custom n="Ashkenaz" fromChapter="3" fromVerse="1" toVerse="19"/>
        <custom n="Sefard" fromChapter="2" fromVerse="20" toChapter="3" toVerse="19"/>
      </haftarah>)
  }

  case object FastOfTammuz extends NonTishaBeAvFast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Tammuz).day(17)

    override val names: Names = parseNames(
      <names>
        <name lang="en">Fast of Tammuz</name>
      </names>)
  }

  case object TishaBeAv extends Fast with PostponeOnShabbos {
    override def date(year: Year): Day = year.month(Av).day(9)
    override def getWeekdayReading: Reading = Reading(torah, haftarah)

    override val names: Names = parseNames(
      <names>
        <name lang="en" transliterated="yes">Tisha BeAv</name>
      </names>)

    private val torah: Torah = parseTorah(
      <torah book="Deuteronomy" fromChapter="4" fromVerse="25" toVerse="40">
        <aliyah n="2" fromVerse="30"/>
        <aliyah n="3" fromVerse="36"/>
      </torah>)

    private val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Jeremiah">
        <custom n="Common" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
        <custom n="Teiman">
          <part n="1" fromChapter="6" fromVerse="16" toVerse="17"/>
          <part n="2" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
        </custom>
      </haftarah>)
  }

  // TODO add number to names
  private def namesWithNumber(withNames: WithNames, number: Int): Names = withNames.names

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

  val daysWithSpecialReadingsNotFestivals: Set[Date] = rabbinicFestivals ++ fasts

  def daysWithSpecialReadings(inHolyLand: Boolean): Set[Date] = festivals(inHolyLand) ++ fasts ++ rabbinicFestivals

  // TODO list all days with haftarah - but why?

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)

  private def parseNames(element: Elem): Names = Names.parse(element, None)

  private def parseTorahForShabbosAndWeekday(element: Elem): (Torah, Torah) = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: ChumashSpan.BookSpan = ChumashSpan.parse(attributes).resolve
    attributes.close()

    val fromChapter: Int = bookSpan.span.from.chapter
    def parseNumberedWithShabbos(attributes: Attributes): (Span.Numbered, Boolean) =
      (parseNumbered(fromChapter)(attributes), attributes.doGetBoolean("shabbos"))

    val result: Seq[(Span.Numbered, Boolean)] =
      elements.map(element => XML.parseEmpty(element, "aliyah", parseNumberedWithShabbos))
    val shabbosAliyot: Torah = Aliyot.parse(bookSpan, result.map(_._1), number = Some(7))
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
    Aliyot.parse(bookSpan, result, number = None)
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

  private def parseHaftarah(element: Elem): Haftarah.Customs = Haftarah.parse(element, full= true)

  private def parseHaftarahNotFull(element: Elem): Haftarah.OptionalCustoms = Haftarah.parseOptional(element, full= false)
}
