package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{From, Parsable, Parser}
import Torah.{Fragment, Maftir}
import org.opentorah.texts.tanach

import scala.xml.Elem

// All the special readings and their rules are here.
// Readings are annotated with their reason.
// This module is date-independent, so the names to use in annotations and date information comes
// from the 'schedule' module: it ties the knot between this and SpecialDay defined in the Jewish calendar.
object SpecialReadings {

  private def parse[R](parsable: Parsable[R], what: String, element: Elem): R =
    Parser.parseDo(parsable.parse(From.xml(what, element)))

  private def parseTorah(element: Elem): Torah = parse(Torah.torahParsable, "Torah", element)

  private def parseMaftir(element: Elem): Maftir = parse(Torah.maftirParsable, "Maftir", element)

  private def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
    parse(Haftarah.haftarahParsable(full), "Haftarah", element)

  private def fromDay(withNames: WithNames, torah: Torah): Torah = torah.fromWithNumbers(withNames)

  private def fromDay(withNames: WithNames, maftir: Maftir): Maftir = maftir.from(withNames)

  private def fromDay(withNames: WithNames, haftarah: Haftarah.Customs): Haftarah.Customs =
    haftarah.map(_.from(withNames), full = false)

  sealed trait WeekdayReading {
    def weekday(day: WithNames): Reading
  }

  sealed trait ShabbosReading {
    def shabbos(day: WithNames): Reading
  }

  sealed trait ShabbosAndWeekdayReading extends ShabbosReading with WeekdayReading {
    final override def weekday(day: WithNames): Reading = getReading(weekdayTorah, day)

    final override def shabbos(day: WithNames): Reading = getReading(shabbosTorah, day)

    private def getReading(torah: Torah, day: WithNames): Reading =
      simpleReading(day, torah, maftir, haftarah)

    protected def shabbosTorah: Torah

    protected def weekdayTorah: Torah

    protected def maftir: Maftir

    protected def haftarah: Haftarah.Customs
  }

  object ErevRoshChodesh {
    def correct(
      day: WithNames,
      isSpecialShabbos: Boolean,
      isRoshChodesh: Boolean,
      isMonthTevesAvElul: Boolean,
      reading: Reading
    ): Reading = {
      val allowReplace: Boolean = !isSpecialShabbos && !isRoshChodesh && ! isMonthTevesAvElul

      def transformer(
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarah: Haftarah,
        addition: Option[Haftarah]
      ): Reading.ReadingCustom =
        if (allowReplace && (custom != Custom.Fes))
          reading.replaceHaftarah(haftarah)
        else
          reading.addHaftarah(addition)

      transformMaftirAndHaftarah(
        day,
        transformer,
        reading,
        shabbosHaftarah,
        shabbosAdditionalHaftarah
      )
    }

    private val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel" fromChapter="20" fromVerse="18" toVerse="42"/>)

    private val shabbosAdditionalHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Chabad, Fes" book="I Samuel" fromChapter="20">
          <part n="1" fromVerse="18" toVerse="18"/>
          <part n="2" fromVerse="42" toVerse="42"/>
        </custom>
      </haftarah>)
  }

  object RoshChodesh extends WeekdayReading {
    def weekday(day: WithNames): Reading = Reading(
      Custom.Ashkenaz -> fromDay(day, ashkenazSefard),
      Custom.Sefard   -> fromDay(day, ashkenazSefard),
      Custom.Hagra    -> fromDay(day, hagra)
    )

    private val all: Seq[Fragment] = parseTorah(
      <torah book="Numbers" fromChapter="28" fromVerse="1" toVerse="15">
        <aliyah n="2" fromVerse="3"/>
        <aliyah n="3" fromVerse="4"/>
        <aliyah n="4" fromVerse="6"/>
        <aliyah n="5" fromVerse="9"/>
        <aliyah n="6" fromVerse="11"/>
      </torah>).spans

    private val (ashkenazSefard: Torah, hagra: Torah) = {
      val aliya1 = all.head+all(1)             // 1-3
      val aliya2AshkenazSefard = all(1)+all(2) // 3-5
      val aliya2Hagra = all(2)+all(3)          // 4-8
      val aliya3 = all(3)+all(4)               // 6-10
      val aliya4 = all(5)                      // 11-15
      val ashkenazSefard = Torah.aliyot(aliya1, aliya2AshkenazSefard, aliya3, aliya4)
      val hagra = Torah.aliyot(aliya1, aliya2Hagra, aliya3, aliya4)
      (ashkenazSefard, hagra)
    }

    def in3aliyot(day: WithNames): Torah = Torah.aliyot(
      (all.head+all(1)+all(2)).from(new Source.AndNumbers(day, 1, 2)), // 1-5
      (all(3)+all(4)         ).from(new Source.AndNumber (day, 3)      ), // 6-10
      all(5)                  .from(new Source.AndNumber (day, 4)      )  // 11-15
    )

    private val shabbosMaftir: Maftir = all(4)+all(5) // 9-15

    def addShabbosMaftirAs7thAliyah(reading: Reading, day: WithNames): Reading =
      reading.transformTorah(_.to6withLast(fromDay(day, shabbosMaftir)))

    def correct(
      day: WithNames,
      isSpecialShabbos: Boolean,
      isMonthTevesAv: Boolean,
      isMonthElul: Boolean,
      reading: Reading
    ): Reading = {
      val allowReplace: Boolean = !isSpecialShabbos && !isMonthTevesAv

      def transformer(
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarah: Haftarah,
        addition: Option[Haftarah]
      ): Reading.ReadingCustom =
        if (allowReplace && (!isMonthElul || (custom == Custom.Chabad)))
          reading.replaceMaftirAndHaftarah(fromDay(day, shabbosMaftir), haftarah)
        else
          reading.addHaftarah(addition)

      transformMaftirAndHaftarah(
        day,
        transformer,
        reading,
        shabbosHaftarah,
        shabbosAdditionalHaftarah
      )
    }

    private val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="66">
        <part n="1" fromVerse="1" toVerse="24"/>
        <part n="2" fromVerse="23" toVerse="23"/>
      </haftarah>)

    private val shabbosAdditionalHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Chabad" book="Isaiah" fromChapter="66">
          <part n="1" fromVerse="1" toVerse="1"/>
          <part n="2" fromVerse="23" toVerse="24"/>
          <part n="3" fromVerse="23" toVerse="23"/>
        </custom>
      </haftarah>)
  }

  object FestivalEnd {
    val shabbosTorah: Torah = parseTorah(
      <torah book="Deuteronomy" fromChapter="14" fromVerse="22" toChapter="16" toVerse="17">
        <aliyah n="2" fromChapter="15" fromVerse="1"/>
        <aliyah n="3" fromChapter="15" fromVerse="19"/>
        <aliyah n="4" fromChapter="16" fromVerse="1"/>
        <aliyah n="5" fromChapter="16" fromVerse="4"/>
        <aliyah n="6" fromChapter="16" fromVerse="9"/>
        <aliyah n="7" fromChapter="16" fromVerse="13"/>
      </torah>)

    val weekdayTorah: Torah = Torah(shabbosTorah.spans.drop(2))
  }

  object IntermediateShabbos {
    val torah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="33" fromVerse="12" toChapter="34" toVerse="26">
        <aliyah n="2" fromChapter="33" fromVerse="17"/>
        <aliyah n="3" fromChapter="33" fromVerse="20"/>
        <aliyah n="4" fromChapter="34" fromVerse="1"/>
        <aliyah n="5" fromChapter="34" fromVerse="4"/>
        <aliyah n="6" fromChapter="34" fromVerse="11"/>
        <aliyah n="7" fromChapter="34" fromVerse="18"/>
      </torah>)
  }

  object RoshHashanah1 extends ShabbosAndWeekdayReading {
    override protected val shabbosTorah: Torah = parseTorah(
      <torah book="Genesis" fromChapter="21" fromVerse="1" toVerse="34">
        <aliyah n="2" fromVerse="5"/>
        <aliyah n="3" fromVerse="9"/>
        <aliyah n="4" fromVerse="13"/>
        <aliyah n="5" fromVerse="18"/>
        <aliyah n="6" fromVerse="22"/>
        <aliyah n="7" fromVerse="28"/>
      </torah>)

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(3, 5))

    override protected val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel" fromChapter="1" fromVerse="1" toChapter="2" toVerse="10"/>)

    override val maftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="29" fromVerse="1" toVerse="6"/>)
  }

  object RoshHashanah2 extends WeekdayReading {
    final override def weekday(day: WithNames): Reading = simpleReading(
      day,
      torah = torah,
      maftir = maftir,
      haftarah = haftarah
    )

    private val torah: Torah = parseTorah(
      <torah book="Genesis" fromChapter="22" fromVerse="1" toVerse="24">
        <aliyah n="2" fromVerse="4"/>
        <aliyah n="3" fromVerse="9"/>
        <aliyah n="4" fromVerse="15"/>
        <aliyah n="5" fromVerse="20"/>
      </torah>)

    private def maftir: Maftir = RoshHashanah1.maftir

    private val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Jeremiah" fromChapter="31" fromVerse="1" toVerse="19"/>)
  }

  object YomKippur extends ShabbosAndWeekdayReading {
    override protected val shabbosTorah: Torah = parseTorah(
      <torah book="Leviticus" fromChapter="16" fromVerse="1" toVerse="34">
        <aliyah n="2" fromVerse="4"/>
        <aliyah n="3" fromVerse="7"/>
        <aliyah n="4" fromVerse="12"/>
        <aliyah n="5" fromVerse="18"/>
        <aliyah n="6" fromVerse="25"/>
        <aliyah n="7" fromVerse="31"/>
      </torah>)

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(2))

    override protected val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah">
        <custom n="Common" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
        <custom n="Italki, Teiman">
          <part n="1" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
          <part n="2" fromChapter="59" fromVerse="20" toVerse="21"/>
        </custom>
      </haftarah>)

    override protected val maftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="29" fromVerse="7" toVerse="11"/>)

    def afternoon(day: WithNames): Reading = Reading(
      torah = fromDay(day, afternoonTorah),
      maftir = None,
      haftarah = fromDay(day, afternoonHaftarah)
    )

    private val afternoonTorah: Torah = parseTorah(
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

  object Succos {
    val korbanot: Seq[Fragment] = parseTorah(
      <torah book="Numbers" fromChapter="29" fromVerse="12" toChapter="30" toVerse="1">
        <aliyah n="2" fromVerse="17"/>
        <aliyah n="3" fromVerse="20"/>
        <aliyah n="4" fromVerse="23"/>
        <aliyah n="5" fromVerse="26"/>
        <aliyah n="6" fromVerse="29"/>
        <aliyah n="7" fromVerse="32"/>
        <aliyah n="8" fromVerse="35"/>
      </torah>).spans
  }

  object Succos1 extends ShabbosAndWeekdayReading {
    override val shabbosTorah: Torah = parseTorah(
      <torah book="Leviticus" fromChapter="22" fromVerse="26" toChapter="23" toVerse="44">
        <aliyah n="2" fromChapter="23" fromVerse="1"/>
        <aliyah n="3" fromChapter="23" fromVerse="4"/>
        <aliyah n="4" fromChapter="23" fromVerse="9"/>
        <aliyah n="5" fromChapter="23" fromVerse="15"/>
        <aliyah n="6" fromChapter="23" fromVerse="23"/>
        <aliyah n="7" fromChapter="23" fromVerse="33"/>
      </torah>)

    override val weekdayTorah: Torah = shabbosTorah.drop(Set(2, 4))

    override val maftir: Maftir = Succos.korbanot.head

    override protected def haftarah: tanach.Haftarah.Customs = parseHaftarah(
      <haftarah book="Zechariah" toChapter="14" toVerse="21">
        <custom n="Common" fromChapter="14" fromVerse="1"/>
        <custom n="Teiman" fromChapter="13" fromVerse="9"/>
      </haftarah>)
  }

  object Succos2 extends ShabbosAndWeekdayReading {
    override protected def shabbosTorah: Torah = Succos1.shabbosTorah

    override protected def weekdayTorah: Torah = Succos1.weekdayTorah

    override protected def maftir: Maftir = Succos1.maftir

    override protected def haftarah: tanach.Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings">
        <custom n="Common" fromChapter="8" fromVerse="2" toVerse="21"/>
        <custom n="Italki" fromChapter="7" fromVerse="51" toChapter="8" toVerse="15"/>
        <custom n="Teiman" fromChapter="7" fromVerse="51" toChapter="8" toVerse="21"/>
      </haftarah>)
  }

  object SuccosIntermediate {

    def weekday(day: WithNames, intermediateDayNumber: Int, inHolyLand: Boolean): Reading = {
      if (intermediateDayNumber == 6) require(inHolyLand)

      // Do not go beyond 6th fragment of korbanot.
      val n: Int = Math.min(intermediateDayNumber, 4)

      val today = korbanotToday(intermediateDayNumber, inHolyLand)

      val ashkenazAndChabad: Torah = Torah.aliyot(
        korbanot(n),
        korbanot(n+1),
        korbanot(n+2),
        today
      )
      val sefard: Torah = Torah.aliyot(today, today, today, today)

      Reading(
        Custom.Ashkenaz -> fromDay(day, ashkenazAndChabad),
        Custom.Chabad -> fromDay(day, ashkenazAndChabad),
        Custom.Sefard -> fromDay(day, sefard)
      )
    }

    def shabbos(day: WithNames, intermediateDayNumber: Int, inHolyLand: Boolean): Reading = Reading(
      torah = fromDay(day, IntermediateShabbos.torah), // TODO maybe from() "IntermediateShabbos"?
      maftir = Some(fromDay(day, korbanotToday(intermediateDayNumber, inHolyLand))),
      haftarah = fromDay(day, shabbosHaftarah)
    )

    private def korbanot(n: Int): Fragment = Succos.korbanot(n)

    private def korbanotToday(n: Int, inHolyLand: Boolean): Maftir =
      if (inHolyLand) korbanot(n) else korbanot(n) + korbanot(n+1)

    private val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="38">
        <custom n="Common" fromVerse="18" toChapter="39" toVerse="16"/>
        <custom n="Italki, Teiman" fromVerse="1" toChapter="38" toVerse="23"/>
      </haftarah>)
  }

  object SheminiAtzeres extends ShabbosAndWeekdayReading {
    override protected val shabbosTorah: Torah = FestivalEnd.shabbosTorah

    override protected val weekdayTorah: Torah = FestivalEnd.shabbosTorah.drop(Set(2, 3))

    override val maftir: Maftir = Succos.korbanot.last

    /* Artscroll gives custom Ashkenaz ending at 9:1,
     but @michaelko58 insists that it is the same as Sefard and ends at 8:66.
     His explanation: "there are some ashkenazic communities that follow custom Italki.
     It is possible that this is a difference between chassidim and litaim." */
    override protected val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings" fromChapter="8" fromVerse="54">
        <custom n="Common" toChapter="8" toVerse="66"/>
        <custom n="Italki, Chabad" toChapter="9" toVerse="1"/>
      </haftarah>)
  }

  object SimchasTorah extends WeekdayReading {
    // TODO do NOT use simpleReading: it overwrites from() on torah, but in this case it should be kept!
    final override def weekday(day: WithNames): Reading = simpleReading(
      day,
      torah(day),
      maftir,
      haftarah
    )

    private val chassanBereishis: Fragment = parseTorah(
      <torah book="Genesis" fromChapter="1" fromVerse="1" toChapter="2" toVerse="3">
        <aliyah n="1" fromVerse="1"/>
      </torah>).spans.head

    private def torah(day: WithNames): Torah =
      fromDay(day, Parsha.VezosHaberachah.days.common)
      .to6withLast(fromDay(day, chassanBereishis))

    private def maftir: Maftir = SheminiAtzeres.maftir

    private val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Joshua">
        <custom n="Common, Italki" fromChapter="1" fromVerse="1" toVerse="18"/>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="9"/>
          <part n="2" fromChapter="6" fromVerse="27"/>
        </custom>
      </haftarah>)
  }


  object SheminiAtzeresAndSimchasTorahInHolyLand extends WeekdayReading with ShabbosReading {
    final override def weekday(day: WithNames): Reading = SimchasTorah.weekday(day)

    override def shabbos(day: WithNames): Reading = weekday(day)
  }

  object Chanukah {
    private def day1Cohen: Torah = SpecialReadings.Chanukah.first
    private def first(n: Int): Fragment = korbanot(2*(n-1))
    private def second(n: Int): Fragment = korbanot(2*(n-1)+1)
    private def zos: Fragment = korbanot.last
    private def split(n: Int): Seq[Fragment] = Seq(first(n), second(n))
    private def full(n: Int): Fragment = first(n)+second(n)

    final def shabbos(
      day: WithNames,
      roshChodeshDay: Option[WithNames],
      dayNumber: Int,
      weeklyReading: WeeklyReading
    ): Reading = {
      val result = replaceMaftirAndHaftarah(weeklyReading.getMorningReading,
        maftir = fromDay(day, full(dayNumber)),
        haftarah = if (dayNumber < 8) shabbos1Haftarah else shabbos2Haftarah)

      roshChodeshDay.fold(result)(roshChodeshDay => RoshChodesh.addShabbosMaftirAs7thAliyah(result, roshChodeshDay))
    }

    final def weekday(
      day: WithNames,
      roshChodeshDay: Option[WithNames],
      dayNumber: Int,
    ): Reading = {
      val (
        ashkenazAndChabad: Seq[Fragment],
        sefard: Seq[Fragment]
        ) = if (dayNumber == 1) {
        val day1CohenAshkenazAndChabad: Fragment = day1Cohen.spans(1)
        val day1CohenSefard: Fragment = day1Cohen.spans.head + day1CohenAshkenazAndChabad
        (
          day1CohenAshkenazAndChabad +: split(dayNumber),
          day1CohenSefard +: split(dayNumber)
        )
      } else if (dayNumber != 8) (
        split(dayNumber) :+ full(dayNumber+1),
        split(dayNumber) :+ full(dayNumber)
      ) else (
        split(dayNumber) :+ zos,
        split(dayNumber) :+ (full(dayNumber) + zos)
      )

      require(ashkenazAndChabad.length == 3)
      require(sefard.length == 3)

      roshChodeshDay.fold(Reading(
        Custom.Ashkenaz -> fromDay(day, Torah(ashkenazAndChabad)),
        Custom.Chabad -> fromDay(day, Torah(ashkenazAndChabad)),
        Custom.Sefard -> fromDay(day, Torah(sefard))
      ))(roshChodeshDay => Reading(RoshChodesh.in3aliyot(roshChodeshDay) :+ fromDay(day, full(dayNumber))))
    }

    private val first: Torah = parseTorah(
      <torah book="Numbers" fromChapter="6" fromVerse="22" toChapter="7" toVerse="11">
        <aliyah n="2" fromChapter="7" fromVerse="1"/>
      </torah>)

    private val korbanot: Seq[Fragment] = parseTorah(
      <torah book="Numbers" fromChapter="7" fromVerse="12" toChapter="8" toVerse="4">
        <aliyah n="1" fromVerse="12"/>
        <aliyah n="2" fromVerse="15"/>
        <aliyah n="3" fromVerse="18"/>
        <aliyah n="4" fromVerse="21"/>
        <aliyah n="5" fromVerse="24"/>
        <aliyah n="6" fromVerse="27"/>
        <aliyah n="7" fromVerse="30"/>
        <aliyah n="8" fromVerse="33"/>
        <aliyah n="9" fromVerse="36"/>
        <aliyah n="10" fromVerse="39"/>
        <aliyah n="11" fromVerse="42"/>
        <aliyah n="12" fromVerse="45"/>
        <aliyah n="13" fromVerse="48"/>
        <aliyah n="14" fromVerse="51"/>
        <aliyah n="15" fromVerse="54"/>
        <aliyah n="16" fromVerse="57"/>
        <aliyah n="17" fromVerse="60"/>
      </torah>).spans

    private val shabbos1Haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Zechariah" fromChapter="2" fromVerse="14" toChapter="4" toVerse="7"/>)

    private val shabbos2Haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings" fromChapter="7" fromVerse="40" toChapter="7">
        <custom n="Common" toVerse="50"/>
        <custom n="Italki" toVerse="51"/>
      </haftarah>)
  }

  sealed trait SpecialParsha {
    final def transform(
      reading: Reading,
      day: WithNames,
      roshChodeshDay: Option[WithNames]
    ): Reading = {
      val result = replaceMaftirAndHaftarah(
        reading,
        maftir = fromDay(day, maftir),
        haftarah = fromDay(day, haftarah)
      )
      roshChodeshDay.fold(result)(roshChodeshDay => RoshChodesh.addShabbosMaftirAs7thAliyah(result, roshChodeshDay))
    }

    protected def maftir: Maftir

    protected def haftarah: Haftarah.Customs
  }

  object ParshasShekalim extends SpecialParsha  {
    override protected val maftir: Maftir = parseMaftir(
      <maftir book="Exodus" fromChapter="30" fromVerse="11" toVerse="16"/>)

    override protected val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="II Kings">
        <custom n="Ashkenaz, Italki, Teiman, Chabad" fromChapter="12" fromVerse="1" toVerse="17"/>
        <custom n="Sefard" fromChapter="11" fromVerse="17" toChapter="12" toVerse="17"/>
      </haftarah>)
  }

  object ParshasZachor extends SpecialParsha {
    override protected val maftir: Maftir = parseMaftir(
      <maftir book="Deuteronomy" fromChapter="25" fromVerse="17" toVerse="19"/>)

    override val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel">
        <custom n="Ashkenaz, Chabad" fromChapter="15" fromVerse="2" toVerse="34"/>
        <custom n="Sefard" fromChapter="15" fromVerse="1" toVerse="34"/>
        <custom n="Teiman" fromChapter="14" fromVerse="52" toChapter="15" toVerse="33"/>
      </haftarah>)
  }

  object Purim extends WeekdayReading {
    override def weekday(day: WithNames): Reading = Reading(fromDay(day, torah))

    val torah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="17" fromVerse="8" toVerse="16">
        <aliyah n="2" fromVerse="11"/>
        <aliyah n="3" fromVerse="14"/>
      </torah>)
  }

  object ShushanPurim extends WeekdayReading {
    override def weekday(day: WithNames): Reading = Reading(fromDay(day, Purim.torah))

    def shabbos(day: WithNames, weeklyReading: WeeklyReading): Reading = replaceMaftirAndHaftarah(
      weeklyReading.getMorningReading,
      maftir = fromDay(day, shabbosMaftir),
      haftarah = ParshasZachor.haftarah // TODO from?
    )

    private val shabbosMaftir: Fragment = Torah.merge(Purim.torah.spans.take(3))
  }

  object ParshasParah extends SpecialParsha {
    override protected val maftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="19" fromVerse="1" toVerse="22"/>)

    override protected val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="36" fromVerse="16">
        <custom n="Ashkenaz, Italki" toVerse="38"/>
        <custom n="Sefard" toVerse="36"/>
      </haftarah>)
  }

  object ParshasHachodesh extends SpecialParsha {
    override protected val maftir: Maftir = parseMaftir(
      <maftir book="Exodus" fromChapter="12" fromVerse="1" toVerse="20"/>)

    override protected val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="45" toChapter="46">
        <custom n="Ashkenaz" fromVerse="16" toVerse="18"/>
        <custom n="Italki" fromVerse="18" toVerse="11"/>
        <custom n="Sefard" fromVerse="18" toVerse="15"/>
        <custom n="Teiman" fromVerse="9" toVerse="11"/>
      </haftarah>)
  }

  object ShabbosHagodol {
    def transform(day: WithNames, isErevPesach: Boolean, reading: Reading): Reading =
      reading.transform[Haftarah](fromDay(day, haftarah), {
        case (custom: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
          if ((custom == Custom.Chabad) && !isErevPesach) readingCustom
          else readingCustom.replaceHaftarah(haftarah)
      })

    private val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Malachi" fromChapter="3" fromVerse="4" toVerse="24"/>)
  }

  object PesachIntermediate extends ShabbosReading {
    final def weekday(day: WithNames, isPesachOnChamishi: Boolean, dayNumber: Int): Reading = {
      val realDayNumber: Int =
        if (isPesachOnChamishi && ((dayNumber == 4) || (dayNumber == 5))) dayNumber-1 else dayNumber
      Reading(fromDay(day, first5(realDayNumber) :+ shabbosMaftir))
    }

    private def first5(realDayNumber: Int): Torah = realDayNumber match {
      case 2 => torah2Intermediate
      case 3 => torah3
      case 4 => torah4
      case 5 => torah5
      case 6 => torah6
    }

    private val torah2Intermediate: Torah = Pesach2.torah.drop(Set(4, 5))

    private val torah3: Torah = parseTorah(
      <torah book="Exodus" fromChapter="13" fromVerse="1" toVerse="16">
        <aliyah n="2" fromVerse="5"/>
        <aliyah n="3" fromVerse="11"/>
      </torah>)

    private val torah4: Torah = parseTorah(
      <torah book="Exodus" fromChapter="22" fromVerse="24" toChapter="23" toVerse="19">
        <aliyah n="2" fromChapter="22" fromVerse="27"/>
        <aliyah n="3" fromChapter="23" fromVerse="6"/>
      </torah>)

    private val torah5: Torah = {
      val all = IntermediateShabbos.torah.spans
      Torah.aliyot(
        all(3),         // Exodus 34:1-3
        all(4)+all(5),  // Exodus 34:4-17
        all(6)          // Exodus 34:18-26
      )
    }

    private val torah6: Torah = parseTorah(
      <torah book="Numbers" fromChapter="9" fromVerse="1" toVerse="14">
        <aliyah n="2" fromVerse="7"/>
        <aliyah n="3" fromVerse="9"/>
      </torah>)

    // Maftir for Pesach Intermediate Shabbos and last two days of Pesach
    val maftirEnd: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="19" toVerse="25"/>)
    private def shabbosMaftir: Maftir = maftirEnd

    private val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel">
        <custom n="Common" fromChapter="37" fromVerse="1" toVerse="14"/>
        <custom n="Teiman" fromChapter="36" fromVerse="37" toChapter="37" toVerse="14"/>
      </haftarah>)

    final override def shabbos(day: WithNames): Reading = Reading(
      torah = fromDay(day, SpecialReadings.IntermediateShabbos.torah), // TODO maybe from() "IntermediateShabbos"?
      maftir = Some(fromDay(day, shabbosMaftir)),
      haftarah = fromDay(day, shabbosHaftarah)
    )
  }

  object Pesach1 extends ShabbosAndWeekdayReading {
    override protected val shabbosTorah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="12" fromVerse="21" toVerse="51">
        <aliyah n="2" fromVerse="25"/>
        <aliyah n="3" fromVerse="29"/>
        <aliyah n="4" fromVerse="33"/>
        <aliyah n="5" fromVerse="37"/>
        <aliyah n="6" fromVerse="43"/>
        <aliyah n="7" fromVerse="48"/>
      </torah>)

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(4, 7))

    override val maftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="16" toVerse="25"/>)

    override protected val haftarah: Haftarah.Customs = parseHaftarah(
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

  object Pesach2 extends WeekdayReading {
    final override def weekday(day: WithNames): Reading = simpleReading(
      day,
      torah = torah,
      maftir = maftir,
      haftarah = haftarah
    )

    val torah: Torah = Succos1.weekdayTorah

    private val maftir: Maftir = Pesach1.maftir

    private val haftarah: Haftarah.Customs = parseHaftarah(
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

  object Pesach7 extends ShabbosAndWeekdayReading {
    override protected val shabbosTorah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="13" fromVerse="17" toChapter="15" toVerse="26">
        <aliyah n="2" fromChapter="13" fromVerse="20"/>
        <aliyah n="3" fromChapter="14" fromVerse="1"/>
        <aliyah n="4" fromChapter="14" fromVerse="5"/>
        <aliyah n="5" fromChapter="14" fromVerse="9"/>
        <aliyah n="6" fromChapter="14" fromVerse="15"/>
        <aliyah n="7" fromChapter="14" fromVerse="26"/>
      </torah>)

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(2, 4))

    override val maftir: Maftir = PesachIntermediate.maftirEnd

    override protected val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="II Samuel" fromChapter="22" fromVerse="1" toVerse="51"/>)
  }

  object Pesach8 extends ShabbosAndWeekdayReading {
    override protected val shabbosTorah: Torah = FestivalEnd.shabbosTorah

    override protected val weekdayTorah: Torah = FestivalEnd.weekdayTorah

    override protected val maftir: Maftir = Pesach7.maftir

    override protected val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="10" fromVerse="32" toChapter="12" toVerse="6"/>)
  }

  object Shavuos1 extends WeekdayReading {
    final override def weekday(day: WithNames): Reading = simpleReading(
      day,
      torah = torah,
      maftir = maftir,
      haftarah = haftarah
    )

    private val torah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="19" fromVerse="1" toChapter="20" toVerse="22">
        <aliyah n="2" fromChapter="19" fromVerse="7"/>
        <aliyah n="3" fromChapter="19" fromVerse="14"/>
        <aliyah n="4" fromChapter="19" fromVerse="20"/>
        <aliyah n="5" fromChapter="20" fromVerse="15"/>
      </torah>)

    val maftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="26" toVerse="31"/>)

    private val haftarah: Haftarah.Customs = parseHaftarah(
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

  object Shavuos2 extends ShabbosAndWeekdayReading {
    override protected val shabbosTorah: Torah = FestivalEnd.shabbosTorah

    override protected val weekdayTorah: Torah = FestivalEnd.weekdayTorah

    override protected def maftir: Maftir = Shavuos1.maftir

    override protected val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Habakkuk">
        <custom n="Ashkenaz" fromChapter="3" fromVerse="1" toVerse="19"/>
        <custom n="Sefard" fromChapter="2" fromVerse="20" toChapter="3" toVerse="19"/>
      </haftarah>)
  }

  object Fast {
    private val afternoonTorahPart1: Fragment = parseTorah(
      <torah book="Exodus" fromChapter="32" fromVerse="11" toVerse="14">
        <aliyah n="1" fromVerse="11"/>
      </torah>).spans.head

    val torah: Torah = Torah.aliyot(
      afternoonTorahPart1,                 // Exodus 32:11-14
      IntermediateShabbos.torah.spans(3),  // Exodus 34:1-3
      IntermediateShabbos.torah.spans(4)   // Exodus 34:4-10
    )

    val defaultAfternoonHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Ashkenaz, Chabad, Morocco" book="Isaiah" fromChapter="55" fromVerse="6" toChapter="56" toVerse="8"/>
        <custom n="Algeria">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>)
  }

  sealed trait Fast extends WeekdayReading {
    def afternoon(day: WithNames): Reading = {
      val torah: Torah = fromDay(day, Fast.torah)
      val fastAfternoonHaftarah: Haftarah.Customs =
        afternoonHaftarahExceptions.fold(Fast.defaultAfternoonHaftarah) { afternoonHaftarahExceptions =>
          Fast.defaultAfternoonHaftarah ++ afternoonHaftarahExceptions }
      val haftarah: Haftarah.Customs = fromDay(day, fastAfternoonHaftarah)
      new Reading(
        customs = haftarah.lift { case (_: Custom, haftarah: Option[Haftarah]) =>
          haftarah.fold(Reading.ReadingCustom(torah, None)) { haftarah: Haftarah =>
            Reading.ReadingCustom(
              torah = Torah(torah.spans),
              maftirAndHaftarah = Some(Reading.MaftirAndHaftarah(None, haftarah))
            )
          }
        }.customs
      )
    }

    protected def afternoonHaftarahExceptions: Option[Haftarah.Customs] = None
  }

  sealed trait NonTishaBeAvFast extends Fast {
    final override def weekday(day: WithNames): Reading = Reading(
      torah = fromDay(day, Fast.torah)
    )
  }

  object FastOfGedalia extends NonTishaBeAvFast {
    override protected val afternoonHaftarahExceptions: Option[Haftarah.Customs] = Some(parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Morocco">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>))
  }

  object FastOfTeves extends NonTishaBeAvFast

  object FastOfEster extends NonTishaBeAvFast

  object FastOfTammuz extends NonTishaBeAvFast

  object TishaBeAv extends Fast {
    final override def weekday(day: WithNames): Reading = Reading(
      torah = fromDay(day, torah),
      maftir = None,
      haftarah = fromDay(day, haftarah)
    )

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

  // TODO stick back into a trait!
  private def simpleReading(
    day: WithNames,
    torah: Torah,
    maftir: Maftir,
    haftarah: Haftarah.Customs
  ): Reading = Reading(
    torah = fromDay(day, torah),
    maftir = Some(fromDay(day, maftir)),
    haftarah = fromDay(day, haftarah)
  )

  private def replaceMaftirAndHaftarah(
    reading: Reading,
    maftir: Maftir,
    haftarah: Haftarah.Customs
  ): Reading = reading.transform[Haftarah](haftarah, transformer = {
    case (_: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
      readingCustom.replaceMaftirAndHaftarah(maftir, haftarah)
  })

  private def transformMaftirAndHaftarah(
    day: WithNames,
    transformer: (
      Custom,
      Reading.ReadingCustom,
      Haftarah,
      Option[Haftarah]
    ) => Reading.ReadingCustom,
    reading: Reading,
    shabbosHaftarah: Haftarah.Customs,
    shabbosAdditionalHaftarah: Haftarah.Customs
  ): Reading = {
    val haftarahs: Custom.Of[(Haftarah, Option[Haftarah])] =
      fromDay(day, shabbosHaftarah) * fromDay(day, shabbosAdditionalHaftarah)

    reading.transform[(Haftarah, Option[Haftarah])](haftarahs, { case (
      custom: Custom,
      reading: Reading.ReadingCustom,
      haftarahs: (Haftarah, Option[Haftarah])
      ) =>
      val (haftarah: Haftarah, addition: Option[Haftarah]) = haftarahs
      transformer(custom, reading, haftarah, addition)
    })
  }
}
