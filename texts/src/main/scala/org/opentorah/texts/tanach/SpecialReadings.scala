package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.podval.metadata.HasNames
import org.podval.xml.{XmlCodec, XmlParser, Xml as ZioXml}
import XmlCodec.given
import Torah.{Fragment, Maftir}
import zio.blocks.schema.{Modifier, Schema}

/* All the special readings and their rules are here.
 Readings are annotated with their reason.
 This module is date-independent, so the names to use in annotations and date information comes
 from the 'schedule' module: it ties the knot between this and SpecialDay defined in the Jewish calendar.

 Some special haftarahs are related to some regular ones etc.
 I am not sure that coding all those relationships will increase clarity, so they are left in the comments.
 */
object SpecialReadings:
  private def parseTorah(element: ZioXml.Element): Torah = Torah.decode(element)

  private def parseMaftir(element: ZioXml.Element): Maftir = Torah.decodeMaftir(element)

  private def parseHaftarah(element: ZioXml.Element, full: Boolean = true): Haftarah.Customs =
    Haftarah.decode(element, full)

  /** A reading in which a custom may read nothing at all; see `reads="none"`. */
  private def parseHaftarahOptional(element: ZioXml.Element): Haftarah.OptionalCustoms =
    Haftarah.decodeOptional(element, full = false)

  /**
   * Identifies one child of a `<day>` in SpecialReadings.xml: the element
   * tag, plus `when` / `role` / `n` when those distinguish extras.
   */
  final case class Slot(
    tag: String,
    when: Option[String] = None,
    role: Option[String] = None,
    n: Option[Int] = None
  ) derives CanEqual

  /**
   * The readings live in SpecialReadings.xml, keyed by the occasion and by
   * [[Slot]]. What is left here is the logic that chooses among them. Lazy,
   * so that loading happens on first use rather than during the initialisation
   * of the objects below.
   */
  @Modifier.config(XmlCodec.IgnoreUnknown, "")
  private final case class DayDto(
    @Modifier.config(XmlCodec.Attribute, "") n: String,
    torah: Seq[ZioXml.Element] = Seq.empty,
    maftir: Seq[ZioXml.Element] = Seq.empty,
    haftarah: Seq[ZioXml.Element] = Seq.empty
  ) derives CanEqual

  private object DayDto:
    given schema: Schema[DayDto] = Schema.derived
    val codec: XmlCodec[DayDto] = XmlCodec.derived

  private lazy val readings: Map[(String, Slot), (ZioXml.Element, Boolean)] =
    val days: Seq[DayDto] =
      XmlParser.loadCatalog(this, "SpecialReadings", DayDto.codec, "specialReadings")
    val parsed: Seq[((String, Slot), (ZioXml.Element, Boolean))] = days.flatMap: day =>
      (day.torah ++ day.maftir ++ day.haftarah).map: element =>
        (day.n, slotOf(element)) -> (element, !element.get("partial").contains("true"))
    Collections.checkNoDuplicates(parsed.map(_._1), "special readings")
    parsed.toMap

  /**
   * What the special readings say about themselves: sources, comments and
   * variants, keyed by the day and the slot, which is what identifies one
   * uniquely. They cannot be keyed by SpecialDay: five of the readings are
   * shared -- Chanukah by its eight days, the intermediate days of Succos and
   * Pesach by theirs, one table by all the fasts.
   */
  lazy val recorded: Map[(String, Slot), Haftarah.Recorded] = readings
    .collect { case (key, (element, full)) if element.isNamed("haftarah") =>
      key -> Haftarah.decodeRecorded(element, full)
    }
    .filterNot((_, recorded) => recorded.isEmpty)

  private def slotOf(element: ZioXml.Element): Slot = Slot(
    tag = element.getName.localName,
    when = element.get("when").map(_.trim).filter(_.nonEmpty),
    role = element.get("role").map(_.trim).filter(_.nonEmpty),
    n = element.get("n").map(_.trim).filter(_.nonEmpty).map(_.toInt)
  )

  private def readingFor(
    day: String,
    tag: String,
    when: Option[String] = None,
    role: Option[String] = None,
    n: Option[Int] = None
  ): (ZioXml.Element, Boolean) =
    val slot: Slot = Slot(tag, when, role, n)
    readings.getOrElse(
      (day, slot),
      throw IllegalArgumentException(s"SpecialReadings.xml has no $slot for '$day'")
    )

  private def torahFor(
    day: String,
    when: Option[String] = None,
    role: Option[String] = None,
    n: Option[Int] = None
  ): Torah = parseTorah(readingFor(day, "torah", when, role, n)._1)

  private def maftirFor(day: String, role: Option[String] = None): Maftir =
    parseMaftir(readingFor(day, "maftir", role = role)._1)

  private def haftarahFor(
    day: String,
    when: Option[String] = None,
    role: Option[String] = None,
    n: Option[Int] = None
  ): Haftarah.Customs =
    val (element: ZioXml.Element, full: Boolean) = readingFor(day, "haftarah", when, role, n)
    parseHaftarah(element, full)

  private def haftarahOptionalFor(
    day: String,
    when: Option[String] = None,
    role: Option[String] = None
  ): Haftarah.OptionalCustoms =
    parseHaftarahOptional(readingFor(day, "haftarah", when, role)._1)

  private def fromDay(named: HasNames, torah: Torah): Torah = torah.fromWithNumbers(named)

  private def fromDay(named: HasNames, maftir: Maftir): Maftir = maftir.from(named)

  private def fromDay(named: HasNames, haftarah: Haftarah.Customs): Haftarah.Customs =
    haftarah.map(_.from(named), full = false)

  private def fromDayOptional(named: HasNames, haftarah: Haftarah.OptionalCustoms): Haftarah.OptionalCustoms =
    haftarah.map(_.map(_.from(named)), full = false)

  sealed trait WeekdayReading:
    def weekday(day: HasNames): Reading

  sealed trait ShabbosReading:
    def shabbos(day: HasNames): Reading

  sealed trait AfternoonReading:
    def afternoon(day: HasNames): Reading

  /**
   * Read at night, after maariv. Only Simchas Torah has one, and only some
   * read it -- so the customs that do not read are given None at the root
   * rather than left out. The map stays full: every custom resolves, and
   * "reads nothing" is an answer rather than a hole. A Reading cannot say
   * this, since it must give every custom a Torah; and there is no maftir or
   * haftarah at night anyway.
   */
  sealed trait EveningReading:
    def evening(day: HasNames): Evening

  /**
   * A night reading. The practices are nested rather than alternative, so the
   * longer one is given and the note says where the shorter stops: showing all
   * five aliyot and saying that some read only the first three tells the whole
   * of it, where showing three and mentioning a longer one would not.
   */
  final class Evening(
    val torah: Custom.Of[Option[Torah]],
    /** Whom the reading belongs to, how far the shorter practice goes, and
      * what settles it. Meant to be shown beside the reading. */
    val note: String
  )

  sealed trait ShabbosAndWeekdayReading extends ShabbosReading, WeekdayReading:
    final override def weekday(day: HasNames): Reading = getReading(weekdayTorah, day)

    final override def shabbos(day: HasNames): Reading = getReading(shabbosTorah, day)

    private def getReading(torah: Torah, day: HasNames): Reading =
      simpleReading(day, torah, maftir, haftarah)

    protected def shabbosTorah: Torah

    protected def weekdayTorah: Torah

    protected def maftir: Maftir

    protected def haftarah: Haftarah.Customs

  object ErevRoshChodesh:
    def correct(
      day: HasNames,
      isSpecialShabbos: Boolean,
      isRoshChodesh: Boolean,
      isMonthTevesAvElul: Boolean,
      isMonthTishrei: Boolean,
      reading: Reading
    ): Reading = if isMonthTishrei then reading else
      // We do not mention Erev Rosh Chodesh on Rosh Hashanah

      val allowReplace: Boolean = !isSpecialShabbos && !isRoshChodesh && ! isMonthTevesAvElul

      def transformer(
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarah: Haftarah,
        addition: Option[Haftarah]
      ): Reading.ReadingCustom =
        if allowReplace && (custom != Custom.Fes)
        then reading.replaceHaftarah(haftarah)
        else reading.addHaftarah(addition)

      transformMaftirAndHaftarah(
        day,
        transformer,
        reading,
        shabbosHaftarah,
        shabbosAdditionalHaftarah
      )

    private val shabbosHaftarah: Haftarah.Customs =
      haftarahFor("ErevRoshChodesh", when = Some("shabbos"))

    private val shabbosAdditionalHaftarah: Haftarah.Customs =
      haftarahFor("ErevRoshChodesh", when = Some("shabbos"), role = Some("additional"))

  object RoshChodesh extends WeekdayReading:
    // Ashkenaz and Sefard divide these aliyot the same way, so the division is
    // stated once for Common: naming the two of them instead would leave any
    // custom that hangs off Common directly -- Romania -- without a reading.
    def weekday(day: HasNames): Reading = readingByCutom(day,
      Custom.Common -> ashkenazSefard,
      Custom.Hagra  -> hagra
    )

    private val torah: Seq[Fragment] = torahFor("RoshChodesh").spans

    private val (ashkenazSefard: Torah, hagra: Torah) =
      val aliya1 = torah.head+torah(1)             // 1-3
      val aliya2AshkenazSefard = torah(1)+torah(2) // 3-5
      val aliya2Hagra = torah(2)+torah(3)          // 4-8
      val aliya3 = torah(3)+torah(4)               // 6-10
      val aliya4 = torah(5)                        // 11-15
      val ashkenazSefard = Torah.aliyot(aliya1, aliya2AshkenazSefard, aliya3, aliya4)
      val hagra = Torah.aliyot(aliya1, aliya2Hagra, aliya3, aliya4)
      (ashkenazSefard, hagra)

    def in3aliyot(day: HasNames): Torah = Torah.aliyot(
      (torah.head+torah(1)+torah(2)).from(day.andNumbers(1, 2)     ), // 1-5
      (torah(3)+torah(4)           ).from(day.andNumber (3)), // 6-10
      torah(5)                      .from(day.andNumber (4))  // 11-15
    )

    private val shabbosMaftir: Maftir = torah(4)+torah(5) // 9-15

    def addShabbosMaftirAs7thAliyah(reading: Reading, day: HasNames): Reading =
      reading.transformTorah(torah => to6withLast(torah, fromDay(day, shabbosMaftir)))

    def correct(
      day: HasNames,
      isSpecialShabbos: Boolean,
      isMonthTeves: Boolean,
      isMonthAv: Boolean,
      isMonthElul: Boolean,
      isMonthTishrei: Boolean,
      reading: Reading
    ): Reading = if isMonthTishrei then reading else
      // We do not mention Rosh Chodesh on Rosh Hashanah

      // The Rosh Chodesh maftir is read unless something else has already taken
      // the maftir, in which case Rosh Chodesh is read as the seventh aliyah
      // instead: on a special Shabbos the four parshiyos take it, and in Teves
      // -- where Rosh Chodesh always falls in Chanukah -- the korbanot do.
      //
      // chabad.org on the Rosh Chodesh Torah reading (ReadingSources
      // chabad-rosh-chodesh-torah) states it without qualification: the weekly
      // portion in seven aliyot as usual, and Numbers 28:9-15 after it for
      // maftir. It names no month it does not hold in, and it treats Rosh
      // Chodesh Teves as the case where a second scroll is taken -- which is
      // the shape of the exception this line makes.
      val allowReplaceMaftir: Boolean = !isSpecialShabbos && !isMonthTeves

      // The haftarah yields more often than the maftir does. In Av the rebuke
      // is read and in Elul the consolation, both in sequence with the weeks
      // around them, and neither gives way to Rosh Chodesh; Chabad is the
      // exception in Elul. Where the Rosh Chodesh haftarah is not read, its
      // closing verses may still be added to the one that is.
      //
      // chabad.org's list of haftarot where customs vary (ReadingSources
      // chabad-haftarot) attests both halves for Chabad. For Av it gives
      // Shim'u, and its footnote records the Rebbe Rashab reading Hashamayim
      // Kis'i one such year and Shim'u when the year came round again,
      // regretting the first choice. For Elul it says that when Re'eh falls on
      // Shabbos Rosh Chodesh the Rosh Chodesh haftarah is read, and that Ki
      // Seitzei then takes Aniya So'ara after its own -- which is what
      // correctKiSeitzei does, two weeks being the distance between them.
      val allowReplaceHaftarah: Boolean = allowReplaceMaftir && !isMonthAv

      def transformer(
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarah: Haftarah,
        addition: Option[Haftarah]
      ): Reading.ReadingCustom =
        if allowReplaceHaftarah && (!isMonthElul || (custom == Custom.Chabad))
        then reading.replaceMaftirAndHaftarah(fromDay(day, shabbosMaftir), haftarah)
        else
          val withMaftir: Reading.ReadingCustom =
            if allowReplaceMaftir then reading.replaceMaftir(fromDay(day, shabbosMaftir)) else reading
          withMaftir.addHaftarah(addition)

      transformMaftirAndHaftarah(
        day,
        transformer,
        reading,
        shabbosHaftarah,
        shabbosAdditionalHaftarah
      )

    private val shabbosHaftarah: Haftarah.Customs =
      haftarahFor("RoshChodesh", when = Some("shabbos"))

    private val shabbosAdditionalHaftarah: Haftarah.Customs =
      haftarahFor("RoshChodesh", when = Some("shabbos"), role = Some("additional"))

  private object FestivalEnd:
    val shabbosTorah: Torah = torahFor("FestivalEnd", when = Some("shabbos"))

    val weekdayTorah: Torah = Torah(shabbosTorah.spans.drop(2))

  private object IntermediateShabbos:
    val torah: Torah = torahFor("IntermediateShabbos")

  object RoshHashanah1 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = torahFor("RoshHashanah1", when = Some("shabbos"))

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(3, 5))

    override val maftir: Maftir = maftirFor("RoshHashanah1")

    override protected val haftarah: Haftarah.Customs = haftarahFor("RoshHashanah1")

  object RoshHashanah2 extends WeekdayReading:
    final override def weekday(day: HasNames): Reading = simpleReading(
      day,
      torah = torah,
      maftir = maftir,
      haftarah = haftarah
    )

    private val torah: Torah = torahFor("RoshHashanah2")

    private def maftir: Maftir = RoshHashanah1.maftir

    private val haftarah: Haftarah.Customs = haftarahFor("RoshHashanah2")

  object YomKippur extends ShabbosAndWeekdayReading, AfternoonReading:
    override protected val shabbosTorah: Torah = torahFor("YomKippur", when = Some("shabbos"))

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(2))

    override protected val haftarah: Haftarah.Customs = haftarahFor("YomKippur")

    override protected val maftir: Maftir = maftirFor("YomKippur")

    override def afternoon(day: HasNames): Reading = Reading(
      torah = fromDay(day, afternoonTorah),
      maftir = None,
      haftarah = fromDay(day, afternoonHaftarah)
    )

    private val afternoonTorah: Torah = torahFor("YomKippur", when = Some("afternoon"))

    private val afternoonHaftarah: Haftarah.Customs = haftarahFor("YomKippur", when = Some("afternoon"))

  object Succos:
    val korbanot: Seq[Fragment] = torahFor("Succos", role = Some("korbanot")).spans

  object Succos1 extends ShabbosAndWeekdayReading:
    override val shabbosTorah: Torah = torahFor("Succos1", when = Some("shabbos"))

    override val weekdayTorah: Torah = shabbosTorah.drop(Set(2, 4))

    override val maftir: Maftir = Succos.korbanot.head

    override protected def haftarah: Haftarah.Customs = haftarahFor("Succos1")

  object Succos2 extends ShabbosAndWeekdayReading:
    override protected def shabbosTorah: Torah = Succos1.shabbosTorah

    override protected def weekdayTorah: Torah = Succos1.weekdayTorah

    override protected def maftir: Maftir = Succos1.maftir

    // Note on the Teiman line: Pekudei Ashkenaz, Chabad
    override protected def haftarah: Haftarah.Customs = haftarahFor("Succos2")

  object SuccosIntermediate:

    def weekday(day: HasNames, intermediateDayNumber: Int, inHolyLand: Boolean): Reading =
      if intermediateDayNumber == 6 then require(inHolyLand)

      // Do not go beyond 6th fragment of korbanot.
      val n: Int = Math.min(intermediateDayNumber, 4)

      val today: Fragment = korbanotToday(intermediateDayNumber, inHolyLand)
      val ashkenazAndChabad: Torah = Torah.aliyot(korbanot(n), korbanot(n+1), korbanot(n+2), today)
      val sefard: Torah = Torah.aliyot(today, today, today, today)

      // Chabad follows Ashkenaz here rather than Sefard, so it keeps its own
      // entry; the Ashkenaz division is stated for Common so that a custom
      // hanging off Common directly -- Romania -- has one too.
      readingByCutom(day,
        Custom.Common -> ashkenazAndChabad,
        Custom.Chabad -> ashkenazAndChabad,
        Custom.Sefard -> sefard
      )

    def shabbos(day: HasNames, intermediateDayNumber: Int, inHolyLand: Boolean): Reading = simpleReading(
      day,
      torah = IntermediateShabbos.torah,
      maftir = korbanotToday(intermediateDayNumber, inHolyLand),
      haftarah = shabbosHaftarah
    )

    private def korbanot(n: Int): Fragment = Succos.korbanot(n)

    private def korbanotToday(n: Int, inHolyLand: Boolean): Maftir =
      if inHolyLand then korbanot(n) else korbanot(n) + korbanot(n+1)

    private val shabbosHaftarah: Haftarah.Customs =
      haftarahFor("SuccosIntermediate", when = Some("shabbos"))

  object SheminiAtzeres extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = FestivalEnd.shabbosTorah

    override protected val weekdayTorah: Torah = FestivalEnd.shabbosTorah.drop(Set(2, 3))

    override val maftir: Maftir = Succos.korbanot.last

    /* Artscroll gives custom Ashkenaz ending at 9:1,
     but @michaelko58 insists that it is the same as Sefard and ends at 8:66.
     His explanation: "there are some ashkenazic communities that follow custom Italki.
     It is possible that this is a difference between chassidim and litaim." */
    override protected val haftarah: Haftarah.Customs = haftarahFor("SheminiAtzeres")

  object SimchasTorah extends WeekdayReading, EveningReading:
    final override def weekday(day: HasNames): Reading = Reading(
      torah(day),
      Some(fromDay(day, maftir)),
      fromDay(day, haftarah)
    )

    private val chassanBereishis: Fragment =
      torahFor("SimchasTorah", role = Some("chassanBereishis")).spans.head

    private def torah(day: HasNames): Torah =
      to6withLast(fromDay(day, Parsha.VezosHaberachah.days.common), fromDay(day, chassanBereishis))

    private def maftir: Maftir = SheminiAtzeres.maftir

    private def haftarah: Haftarah.Customs = Parsha.VezosHaberachah.haftarah

    /**
     * The night reading, after the hakafos: Ashkenaz only, and not all of
     * them. Some read the first three aliyot of Vezos Haberachah and some the
     * first five; the split does not follow rite lines and is fragmented, so
     * both are recorded rather than one being chosen. Nitei Gavriel, Hilchos
     * Sukkos. Not a Custom.Of variant: the two belong to the same custom, and
     * the Torah readings carry no variant of their own the way haftarot do.
     */
    override def evening(day: HasNames): Evening = Evening(
      torah = eveningAliyot(day, 5),
      note = "Read by Ashkenaz, and not by all of them. Some read only the " +
             "first three of these aliyot. The split does not follow rite " +
             "lines. Nitei Gavriel, Hilchos Sukkos."
    )

    private def eveningAliyot(day: HasNames, count: Int): Custom.Of[Option[Torah]] =
      val ashkenaz: Torah = Parsha.VezosHaberachah.days.doFind(Custom.Ashkenaz)
      val torah: Torah = fromDay(day, Torah(ashkenaz.spans.take(count)))
      Custom.Of(Map(Custom.Common -> None, Custom.Ashkenaz -> Some(torah)))


  object SheminiAtzeresAndSimchasTorahInHolyLand extends WeekdayReading, ShabbosReading:
    final override def weekday(day: HasNames): Reading = SimchasTorah.weekday(day)

    override def shabbos(day: HasNames): Reading = weekday(day)

  object Chanukah:
    final def shabbos(
      day: HasNames,
      roshChodeshDay: Option[HasNames],
      dayNumber: Int,
      weeklyReading: WeeklyReading
    ): Reading =
      val result = replaceMaftirAndHaftarah(weeklyReading.getMorningReading,
        maftir = fromDay(day, full(dayNumber)),
        haftarah = if dayNumber < 8 then shabbos1Haftarah else shabbos2Haftarah)

      roshChodeshDay.fold(result)(roshChodeshDay => RoshChodesh.addShabbosMaftirAs7thAliyah(result, roshChodeshDay))

    final def weekday(
      day: HasNames,
      roshChodeshDay: Option[HasNames],
      dayNumber: Int,
    ): Reading =
      val (
        ashkenazAndChabad: Seq[Fragment],
        sefard: Seq[Fragment]
        ) = if dayNumber == 1 then
        val day1CohenAshkenazAndChabad: Fragment = day1Cohen.spans(1)
        val day1CohenSefard: Fragment = day1Cohen.spans.head + day1CohenAshkenazAndChabad
        (
          day1CohenAshkenazAndChabad +: split(dayNumber),
          day1CohenSefard +: split(dayNumber)
        )
      else if dayNumber != 8 then (
        split(dayNumber) :+ full(dayNumber+1),
        split(dayNumber) :+ full(dayNumber)
      ) else (
        split(dayNumber) :+ zos,
        split(dayNumber) :+ (full(dayNumber) + zos)
      )

      require(ashkenazAndChabad.length == 3)
      require(sefard.length == 3)

      roshChodeshDay.fold(readingByCutom(day,
        Custom.Common -> Torah(ashkenazAndChabad),
        Custom.Chabad -> Torah(ashkenazAndChabad),
        Custom.Sefard -> Torah(sefard)
      ))(roshChodeshDay => Reading(RoshChodesh.in3aliyot(roshChodeshDay) :+ fromDay(day, full(dayNumber))))

    private def first(n: Int): Fragment = korbanot(2*(n-1))
    private def second(n: Int): Fragment = korbanot(2*(n-1)+1)
    private def split(n: Int): Seq[Fragment] = Seq(first(n), second(n))
    private def full(n: Int): Fragment = first(n)+second(n)
    private def zos: Fragment = korbanot.last

    private def day1Cohen: Torah = torahFor("Chanukah", role = Some("day1Cohen"))

    private val korbanot: Seq[Fragment] = torahFor("Chanukah", role = Some("korbanot")).spans

    private val shabbos1Haftarah: Haftarah.Customs =
      haftarahFor("Chanukah", when = Some("shabbos"), n = Some(1)) // = Beha'aloscha Common

    private val shabbos2Haftarah: Haftarah.Customs =
      haftarahFor("Chanukah", when = Some("shabbos"), n = Some(2))

  sealed trait SpecialParsha:
    final def transform(
      reading: Reading,
      day: HasNames,
      roshChodeshDay: Option[HasNames]
    ): Reading =
      val result = replaceMaftirAndHaftarah(
        reading,
        maftir = fromDay(day, maftir),
        haftarah = fromDay(day, haftarah)
      )
      roshChodeshDay.fold(result)(roshChodeshDay => RoshChodesh.addShabbosMaftirAs7thAliyah(result, roshChodeshDay))

    protected def maftir: Maftir

    protected def haftarah: Haftarah.Customs

  object ParshasShekalim extends SpecialParsha :
    override protected val maftir: Maftir = maftirFor("ParshasShekalim")

    override protected val haftarah: Haftarah.Customs = haftarahFor("ParshasShekalim")

  object ParshasZachor extends SpecialParsha:
    override protected val maftir: Maftir = maftirFor("ParshasZachor")

    override val haftarah: Haftarah.Customs = haftarahFor("ParshasZachor")

  object Purim extends WeekdayReading:
    override def weekday(day: HasNames): Reading = Reading(fromDay(day, torah))

    val torah: Torah = torahFor("Purim")

  object ShushanPurim extends WeekdayReading:
    override def weekday(day: HasNames): Reading = Reading(fromDay(day, Purim.torah))

    def shabbos(day: HasNames, weeklyReading: WeeklyReading): Reading = replaceMaftirAndHaftarah(
      weeklyReading.getMorningReading,
      maftir = fromDay(day, shabbosMaftir),
      haftarah = ParshasZachor.haftarah
    )

    private val shabbosMaftir: Fragment = Torah.merge(Purim.torah.spans.take(3))

  object ParshasParah extends SpecialParsha:
    override protected val maftir: Maftir = maftirFor("ParshasParah")

    override protected val haftarah: Haftarah.Customs = haftarahFor("ParshasParah")

  object ParshasHachodesh extends SpecialParsha:
    override protected val maftir: Maftir = maftirFor("ParshasHachodesh")

    override protected val haftarah: Haftarah.Customs = haftarahFor("ParshasHachodesh")

  object ShabbosHagodol:
    def transform(day: HasNames, isErevPesach: Boolean, reading: Reading): Reading =
      reading.transform[Haftarah](fromDay(day, haftarah),
        (custom: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
          if (custom == Custom.Chabad) && !isErevPesach then readingCustom
          else readingCustom.replaceHaftarah(haftarah)
      )

    private val haftarah: Haftarah.Customs = haftarahFor("ShabbosHagodol")

  object PesachIntermediate extends ShabbosReading:
    final def weekday(day: HasNames, isPesachOnChamishi: Boolean, dayNumber: Int): Reading =
      val realDayNumber: Int =
        if isPesachOnChamishi && ((dayNumber == 4) || (dayNumber == 5)) then dayNumber-1 else dayNumber
      Reading(fromDay(day, first5(realDayNumber) :+ shabbosMaftir))

    final override def shabbos(day: HasNames): Reading = simpleReading(
      day,
      torah = IntermediateShabbos.torah,
      maftir = shabbosMaftir,
      haftarah = shabbosHaftarah
    )

    private def first5(realDayNumber: Int): Torah = realDayNumber match
      case 2 => torah2Intermediate
      case 3 => torah3
      case 4 => torah4
      case 5 => torah5
      case 6 => torah6

    private val torah2Intermediate: Torah = Pesach2.torah.drop(Set(4, 5))

    private val torah3: Torah = torahFor("PesachIntermediate", n = Some(3))

    private val torah4: Torah = torahFor("PesachIntermediate", n = Some(4))

    private val torah5: Torah =
      val all = IntermediateShabbos.torah.spans
      Torah.aliyot(
        all(3),         // Exodus 34:1-3
        all(4)+all(5),  // Exodus 34:4-17
        all(6)          // Exodus 34:18-26
      )

    private val torah6: Torah = torahFor("PesachIntermediate", n = Some(6))

    // Maftir for Pesach Intermediate Shabbos and last two days of Pesach
    val maftirEnd: Maftir = maftirFor("PesachIntermediate", role = Some("end"))
    private def shabbosMaftir: Maftir = maftirEnd

    private val shabbosHaftarah: Haftarah.Customs =
      haftarahFor("PesachIntermediate", when = Some("shabbos"))

  object Pesach1 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = torahFor("Pesach1", when = Some("shabbos"))

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(4, 7))

    override val maftir: Maftir = maftirFor("Pesach1")

    // Piece 2 below is also Vezos Haberachah/Simchas Torah Teiman part 2.
    override protected val haftarah: Haftarah.Customs = haftarahFor("Pesach1")

  object Pesach2 extends WeekdayReading:
    final override def weekday(day: HasNames): Reading = simpleReading(
      day,
      torah = torah,
      maftir = maftir,
      haftarah = haftarah
    )

    val torah: Torah = Succos1.weekdayTorah

    private val maftir: Maftir = Pesach1.maftir

    private val haftarah: Haftarah.Customs = haftarahFor("Pesach2")

  object Pesach7 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = torahFor("Pesach7", when = Some("shabbos"))

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(2, 4))

    override val maftir: Maftir = PesachIntermediate.maftirEnd

    override protected val haftarah: Haftarah.Customs = haftarahFor("Pesach7")

  object Pesach8 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = FestivalEnd.shabbosTorah

    override protected val weekdayTorah: Torah = FestivalEnd.weekdayTorah

    override protected val maftir: Maftir = Pesach7.maftir

    override protected val haftarah: Haftarah.Customs = haftarahFor("Pesach8")

  object Shavuos1 extends WeekdayReading:
    final override def weekday(day: HasNames): Reading = simpleReading(
      day,
      torah = torah,
      maftir = maftir,
      haftarah = haftarah
    )

    private val torah: Torah = torahFor("Shavuos1")

    val maftir: Maftir = maftirFor("Shavuos1")

    private val haftarah: Haftarah.Customs = haftarahFor("Shavuos1")

  object Shavuos2 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = FestivalEnd.shabbosTorah

    override protected val weekdayTorah: Torah = FestivalEnd.weekdayTorah

    override protected def maftir: Maftir = Shavuos1.maftir

    override protected val haftarah: Haftarah.Customs = haftarahFor("Shavuos2")

  object Fast:
    private val afternoonTorahPart1: Fragment =
      torahFor("Fast", when = Some("afternoon"), role = Some("part1")).spans.head

    val torah: Torah = Torah.aliyot(
      afternoonTorahPart1,                 // Exodus 32:11-14
      IntermediateShabbos.torah.spans(3),  // Exodus 34:1-3
      IntermediateShabbos.torah.spans(4)   // Exodus 34:4-10
    )

    val defaultAfternoonHaftarah: Haftarah.OptionalCustoms =
      haftarahOptionalFor("Fast", when = Some("afternoon"), role = Some("default"))

  sealed trait Fast extends WeekdayReading, AfternoonReading:
    override def afternoon(day: HasNames): Reading =
      val torah: Torah = fromDay(day, Fast.torah)
      val haftarah: Haftarah.OptionalCustoms = fromDayOptional(day, afternoonHaftarah)
      new Reading(
        customs = haftarah.lift[Reading.ReadingCustom]((_: Custom, found: Option[Option[Haftarah]]) =>
          // Some(None) is a custom that reads nothing; None is one with no entry
          found.flatten.fold(Reading.ReadingCustom(torah, None))((haftarah: Haftarah) =>
            Reading.ReadingCustom(
              torah = Torah(torah.spans),
              maftirAndHaftarah = Some(Reading.MaftirAndHaftarah(None, haftarah))
            )
          )
        ).customs
      )

    /**
     * What is read at Mincha. Most fasts share one table and differ from it in
     * places, which is what afternoonHaftarahExceptions is for; Tisha BeAv has
     * a table of its own, and overrides this instead. Layering it on the shared
     * one would be wrong rather than merely verbose: the exceptions can add and
     * replace but not take away, and the customs that read nothing on an
     * ordinary fast do read on Tisha BeAv.
     */
    protected def afternoonHaftarah: Haftarah.OptionalCustoms =
      afternoonHaftarahExceptions.fold(Fast.defaultAfternoonHaftarah)(afternoonHaftarahExceptions =>
        Fast.defaultAfternoonHaftarah ++ afternoonHaftarahExceptions)

    protected def afternoonHaftarahExceptions: Option[Haftarah.OptionalCustoms] = None

  sealed trait NonTishaBeAvFast extends Fast:
    final override def weekday(day: HasNames): Reading = Reading(fromDay(day, Fast.torah))

  object FastOfGedalia extends NonTishaBeAvFast:
    override protected val afternoonHaftarahExceptions: Option[Haftarah.OptionalCustoms] =
      Some(haftarahOptionalFor("FastOfGedalia", when = Some("afternoon"), role = Some("exceptions")))

  object FastOfTeves extends NonTishaBeAvFast

  object FastOfEster extends NonTishaBeAvFast

  object FastOfTammuz extends NonTishaBeAvFast

  object TishaBeAv extends Fast:
    final override def weekday(day: HasNames): Reading = Reading(
      torah = fromDay(day, torah),
      maftir = None,
      haftarah = fromDay(day, haftarah)
    )

    private val torah: Torah = torahFor("TishaBeAv")

    private val haftarah: Haftarah.Customs = haftarahFor("TishaBeAv")

    override protected val afternoonHaftarah: Haftarah.OptionalCustoms =
      haftarahOptionalFor("TishaBeAv", when = Some("afternoon"))

  /**
   * Shabbos Shuvah -- the Shabbos between Rosh Hashanah and Yom Kippur -- has
   * its own haftarah, which Haftarah.xml stores on Vayeilech:
   *   <week n="Vayeilech"> <!-- = Shabbos Shuvah -->
   * That identification holds only in the years when Vayeilech is the parsha
   * of that week. When Nitzavim and Vayeilech are combined they are read
   * *before* Rosh Hashanah, Haazinu falls on Shabbos Shuvah, and it keeps its
   * own haftarah (Shiras David), which belongs to it only in the years when it
   * falls after Yom Kippur. Attach the haftarah to the week rather than to the
   * parsha, so it is read on Shabbos Shuvah either way.
   */
  def correctShabbosShuvah(reading: Reading, isShabbosShuvah: Boolean): Reading =
    if !isShabbosShuvah then reading else reading.transform[Haftarah](
      Parsha.Vayeilech.haftarah,
      (_: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
        readingCustom.replaceHaftarah(haftarah)
    )

  /**
   * The three haftarot of rebuke belong to the three Shabbosim between the
   * Fast of Tammuz and Tisha BeAv, not to the parshiyos they usually fall on.
   * When Mattos and Masei are combined there is one Shabbos fewer, and the
   * sequence shifts a week earlier: Pinchas -- which then falls after the
   * 17th of Tammuz -- reads Mattos's haftarah, and the combined week reads
   * Masei's, as noted in Haftarah.xml:
   *   <week n="Pinchas" ...> <!-- In 3 weeks - Mattos -->
   * Pinchas's own haftarah is read only in the years when it falls before the
   * fast. The shift is stated in terms of the parshiyos rather than of the
   * rebukes, so it holds for the customs that do not follow that scheme.
   */
  def correctPinchas(reading: Reading, isPinchas: Boolean, isAfterFastOfTammuz: Boolean): Reading =
    if !isPinchas || !isAfterFastOfTammuz then reading else reading.transform[Haftarah](
      Parsha.Mattos.haftarah,
      (_: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
        readingCustom.replaceHaftarah(haftarah)
    )

  def correctKiSeitzei(reading: Reading, isMonthElul: Boolean, dayNumber: Int): Reading =
    val isKiSeitzei: Boolean = isMonthElul && (dayNumber == 14)
    if !isKiSeitzei then reading else
      val customs: Custom.Of[Reading.ReadingCustom] = reading.liftR(
        (custom: Custom, readingCustom: Reading.ReadingCustom) =>
          if custom != Custom.Chabad then readingCustom
          else readingCustom.addHaftarah(Parsha.Re_eh.haftarah.doFind(Custom.Chabad))
      )
      new Reading(customs.customs)

  private def to6withLast(torah: Torah, last: Torah.Aliyah): Torah = torah.drop(Set(7)) :+ last

  private def readingByCutom(day: HasNames, customs: (Custom, Torah)*): Reading =
    Reading(Custom.Of(customs.map((custom, torah) => (custom, fromDay(day, torah))).toMap))

  private def simpleReading(
    day: HasNames,
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
  ): Reading = reading.transform[Haftarah](haftarah, transformer =
    (_: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
      readingCustom.replaceMaftirAndHaftarah(maftir, haftarah)
  )

  private def transformMaftirAndHaftarah(
    day: HasNames,
    transformer: (
      Custom,
      Reading.ReadingCustom,
      Haftarah,
      Option[Haftarah]
    ) => Reading.ReadingCustom,
    reading: Reading,
    shabbosHaftarah: Haftarah.Customs,
    shabbosAdditionalHaftarah: Haftarah.Customs
  ): Reading =
    val haftarahs: Custom.Of[(Haftarah, Option[Haftarah])] =
      fromDay(day, shabbosHaftarah) * fromDay(day, shabbosAdditionalHaftarah)

    reading.transform[(Haftarah, Option[Haftarah])](haftarahs, (
      custom: Custom,
      reading: Reading.ReadingCustom,
      haftarahs: (Haftarah, Option[Haftarah])
      ) =>
      val (haftarah: Haftarah, addition: Option[Haftarah]) = haftarahs
      transformer(custom, reading, haftarah, addition)
    )
