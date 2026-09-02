package org.opentorah.texts.tanach

import org.opentorah.metadata.Named
import org.opentorah.util.Collections
import org.opentorah.xml.{Element, ElementsTo, From, Parser}
import Torah.{Fragment, Maftir}

/* All the special readings and their rules are here.
 Readings are annotated with their reason.
 This module is date-independent, so the names to use in annotations and date information comes
 from the 'schedule' module: it ties the knot between this and SpecialDay defined in the Jewish calendar.

 Some special haftarahs are related to some regular ones etc.
 I am not sure that coding all those relationships will increase clarity, so they are left in the comments.
 */
object SpecialReadings:

  private def parseTorah(element: Element): Torah = parse(Torah.torahParsable, "Torah", element)

  private def parseMaftir(element: Element): Maftir = parse(Torah.Maftir, "Maftir", element)

  private def parseHaftarah(element: Element, full: Boolean = true): Haftarah.Customs =
    parse(Haftarah.element(full), "Haftarah", element)

  /** A reading in which a custom may read nothing at all; see Haftarah.NoneElement. */
  private def parseHaftarahOptional(element: Element): Haftarah.OptionalCustoms =
    parse(Haftarah.optionalElement(full = false), "Haftarah", element)

  private def parse[R](fromXml: ElementsTo[R], what: String, element: Element): R =
    Parser.unsafeRun(fromXml.parse(From.xml(what, element)))

  /**
   * The readings live in SpecialReadings.xml, keyed by the occasion and by the
   * name this file knows the reading under, the way Torah.xml and Haftarah.xml
   * are keyed by parsha. What is left here is the logic that chooses among
   * them. Lazy, so that loading happens on first use rather than during the
   * initialisation of the objects below.
   */
  private lazy val readings: Map[(String, String), (Element, Boolean)] =
    val root: Element = Parser.unsafeRun(From.resourceNamed(this, "SpecialReadings").load)
    val parsed: Seq[((String, String), (Element, Boolean))] = for
      day: scala.xml.Node <- (root \ "day").toSeq
      reading: scala.xml.Node <- (day \ "reading").toSeq
      element: Element <- reading.child.collect { case elem: Element => elem }
    yield (day \@ "n", reading \@ "n") -> (element, (reading \@ "full") != "false")
    Collections.checkNoDuplicates(parsed.map(_._1), "special readings")
    parsed.toMap

  private def readingFor(day: String, name: String): (Element, Boolean) = readings.getOrElse(
    (day, name),
    throw IllegalArgumentException(s"SpecialReadings.xml has no '$name' for '$day'")
  )

  private def torahFor(day: String, name: String): Torah = parseTorah(readingFor(day, name)._1)

  private def maftirFor(day: String, name: String): Maftir = parseMaftir(readingFor(day, name)._1)

  private def haftarahFor(day: String, name: String): Haftarah.Customs =
    val (element: Element, full: Boolean) = readingFor(day, name)
    parseHaftarah(element, full)

  private def haftarahOptionalFor(day: String, name: String): Haftarah.OptionalCustoms =
    parseHaftarahOptional(readingFor(day, name)._1)

  private def fromDay(named: Named, torah: Torah): Torah = torah.fromWithNumbers(named)

  private def fromDay(named: Named, maftir: Maftir): Maftir = maftir.from(named)

  private def fromDay(named: Named, haftarah: Haftarah.Customs): Haftarah.Customs =
    haftarah.map(_.from(named), full = false)

  private def fromDayOptional(named: Named, haftarah: Haftarah.OptionalCustoms): Haftarah.OptionalCustoms =
    haftarah.map(_.map(_.from(named)), full = false)

  sealed trait WeekdayReading:
    def weekday(day: Named): Reading

  sealed trait ShabbosReading:
    def shabbos(day: Named): Reading

  sealed trait AfternoonReading:
    def afternoon(day: Named): Reading

  /**
   * Read at night, after maariv. Only Simchas Torah has one, and only some
   * read it -- so the customs that do not read are given None at the root
   * rather than left out. The map stays full: every custom resolves, and
   * "reads nothing" is an answer rather than a hole. A Reading cannot say
   * this, since it must give every custom a Torah; and there is no maftir or
   * haftarah at night anyway.
   */
  sealed trait EveningReading:
    def evening(day: Named): Evening

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
    final override def weekday(day: Named): Reading = getReading(weekdayTorah, day)

    final override def shabbos(day: Named): Reading = getReading(shabbosTorah, day)

    private def getReading(torah: Torah, day: Named): Reading =
      simpleReading(day, torah, maftir, haftarah)

    protected def shabbosTorah: Torah

    protected def weekdayTorah: Torah

    protected def maftir: Maftir

    protected def haftarah: Haftarah.Customs

  object ErevRoshChodesh:
    def correct(
      day: Named,
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

    private val shabbosHaftarah: Haftarah.Customs = haftarahFor("ErevRoshChodesh", "shabbosHaftarah")

    private val shabbosAdditionalHaftarah: Haftarah.Customs = haftarahFor("ErevRoshChodesh", "shabbosAdditionalHaftarah")

  object RoshChodesh extends WeekdayReading:
    // Ashkenaz and Sefard divide these aliyot the same way, so the division is
    // stated once for Common: naming the two of them instead would leave any
    // custom that hangs off Common directly -- Romania -- without a reading.
    def weekday(day: Named): Reading = readingByCutom(day,
      Custom.Common -> ashkenazSefard,
      Custom.Hagra  -> hagra
    )

    private val torah: Seq[Fragment] = torahFor("RoshChodesh", "torah").spans

    private val (ashkenazSefard: Torah, hagra: Torah) =
      val aliya1 = torah.head+torah(1)             // 1-3
      val aliya2AshkenazSefard = torah(1)+torah(2) // 3-5
      val aliya2Hagra = torah(2)+torah(3)          // 4-8
      val aliya3 = torah(3)+torah(4)               // 6-10
      val aliya4 = torah(5)                        // 11-15
      val ashkenazSefard = Torah.aliyot(aliya1, aliya2AshkenazSefard, aliya3, aliya4)
      val hagra = Torah.aliyot(aliya1, aliya2Hagra, aliya3, aliya4)
      (ashkenazSefard, hagra)

    def in3aliyot(day: Named): Torah = Torah.aliyot(
      (torah.head+torah(1)+torah(2)).from(day.andNumbers(1, 2)     ), // 1-5
      (torah(3)+torah(4)           ).from(day.andNumber (3)), // 6-10
      torah(5)                      .from(day.andNumber (4))  // 11-15
    )

    private val shabbosMaftir: Maftir = torah(4)+torah(5) // 9-15

    def addShabbosMaftirAs7thAliyah(reading: Reading, day: Named): Reading =
      reading.transformTorah(torah => to6withLast(torah, fromDay(day, shabbosMaftir)))

    def correct(
      day: Named,
      isSpecialShabbos: Boolean,
      isMonthTevesAv: Boolean,
      isMonthElul: Boolean,
      isMonthTishrei: Boolean,
      reading: Reading
    ): Reading = if isMonthTishrei then reading else
      // We do not mention Rosh Chodesh on Rosh Hashanah
      val allowReplace: Boolean = !isSpecialShabbos && !isMonthTevesAv

      def transformer(
        custom: Custom,
        reading: Reading.ReadingCustom,
        haftarah: Haftarah,
        addition: Option[Haftarah]
      ): Reading.ReadingCustom =
        if allowReplace && (!isMonthElul || (custom == Custom.Chabad))
        then reading.replaceMaftirAndHaftarah(fromDay(day, shabbosMaftir), haftarah)
        else reading.addHaftarah(addition)

      transformMaftirAndHaftarah(
        day,
        transformer,
        reading,
        shabbosHaftarah,
        shabbosAdditionalHaftarah
      )

    private val shabbosHaftarah: Haftarah.Customs = haftarahFor("RoshChodesh", "shabbosHaftarah")

    private val shabbosAdditionalHaftarah: Haftarah.Customs = haftarahFor("RoshChodesh", "shabbosAdditionalHaftarah")

  private object FestivalEnd:
    val shabbosTorah: Torah = torahFor("FestivalEnd", "shabbosTorah")

    val weekdayTorah: Torah = Torah(shabbosTorah.spans.drop(2))

  private object IntermediateShabbos:
    val torah: Torah = torahFor("IntermediateShabbos", "torah")

  object RoshHashanah1 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = torahFor("RoshHashanah1", "shabbosTorah")

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(3, 5))

    override val maftir: Maftir = maftirFor("RoshHashanah1", "maftir")

    override protected val haftarah: Haftarah.Customs = haftarahFor("RoshHashanah1", "haftarah")

  object RoshHashanah2 extends WeekdayReading:
    final override def weekday(day: Named): Reading = simpleReading(
      day,
      torah = torah,
      maftir = maftir,
      haftarah = haftarah
    )

    private val torah: Torah = torahFor("RoshHashanah2", "torah")

    private def maftir: Maftir = RoshHashanah1.maftir

    private val haftarah: Haftarah.Customs = haftarahFor("RoshHashanah2", "haftarah")

  object YomKippur extends ShabbosAndWeekdayReading, AfternoonReading:
    override protected val shabbosTorah: Torah = torahFor("YomKippur", "shabbosTorah")

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(2))

    override protected val haftarah: Haftarah.Customs = haftarahFor("YomKippur", "haftarah")

    override protected val maftir: Maftir = maftirFor("YomKippur", "maftir")

    override def afternoon(day: Named): Reading = Reading(
      torah = fromDay(day, afternoonTorah),
      maftir = None,
      haftarah = fromDay(day, afternoonHaftarah)
    )

    private val afternoonTorah: Torah = torahFor("YomKippur", "afternoonTorah")

    private val afternoonHaftarah: Haftarah.Customs = haftarahFor("YomKippur", "afternoonHaftarah")

  object Succos:
    val korbanot: Seq[Fragment] = torahFor("Succos", "korbanot").spans

  object Succos1 extends ShabbosAndWeekdayReading:
    override val shabbosTorah: Torah = torahFor("Succos1", "shabbosTorah")

    override val weekdayTorah: Torah = shabbosTorah.drop(Set(2, 4))

    override val maftir: Maftir = Succos.korbanot.head

    override protected def haftarah: Haftarah.Customs = haftarahFor("Succos1", "haftarah")

  object Succos2 extends ShabbosAndWeekdayReading:
    override protected def shabbosTorah: Torah = Succos1.shabbosTorah

    override protected def weekdayTorah: Torah = Succos1.weekdayTorah

    override protected def maftir: Maftir = Succos1.maftir

    // Note on the Teiman line: Pekudei Ashkenaz, Chabad
    override protected def haftarah: Haftarah.Customs = haftarahFor("Succos2", "haftarah")

  object SuccosIntermediate:

    def weekday(day: Named, intermediateDayNumber: Int, inHolyLand: Boolean): Reading =
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

    def shabbos(day: Named, intermediateDayNumber: Int, inHolyLand: Boolean): Reading = simpleReading(
      day,
      torah = IntermediateShabbos.torah,
      maftir = korbanotToday(intermediateDayNumber, inHolyLand),
      haftarah = shabbosHaftarah
    )

    private def korbanot(n: Int): Fragment = Succos.korbanot(n)

    private def korbanotToday(n: Int, inHolyLand: Boolean): Maftir =
      if inHolyLand then korbanot(n) else korbanot(n) + korbanot(n+1)

    private val shabbosHaftarah: Haftarah.Customs = haftarahFor("SuccosIntermediate", "shabbosHaftarah")

  object SheminiAtzeres extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = FestivalEnd.shabbosTorah

    override protected val weekdayTorah: Torah = FestivalEnd.shabbosTorah.drop(Set(2, 3))

    override val maftir: Maftir = Succos.korbanot.last

    /* Artscroll gives custom Ashkenaz ending at 9:1,
     but @michaelko58 insists that it is the same as Sefard and ends at 8:66.
     His explanation: "there are some ashkenazic communities that follow custom Italki.
     It is possible that this is a difference between chassidim and litaim." */
    override protected val haftarah: Haftarah.Customs = haftarahFor("SheminiAtzeres", "haftarah")

  object SimchasTorah extends WeekdayReading, EveningReading:
    final override def weekday(day: Named): Reading = Reading(
      torah(day),
      Some(fromDay(day, maftir)),
      fromDay(day, haftarah)
    )

    private val chassanBereishis: Fragment = torahFor("SimchasTorah", "chassanBereishis").spans.head

    private def torah(day: Named): Torah =
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
    override def evening(day: Named): Evening = Evening(
      torah = eveningAliyot(day, 5),
      note = "Read by Ashkenaz, and not by all of them. Some read only the " +
             "first three of these aliyot. The split does not follow rite " +
             "lines. Nitei Gavriel, Hilchos Sukkos."
    )

    private def eveningAliyot(day: Named, count: Int): Custom.Of[Option[Torah]] =
      val ashkenaz: Torah = Parsha.VezosHaberachah.days.doFind(Custom.Ashkenaz)
      val torah: Torah = fromDay(day, Torah(ashkenaz.spans.take(count)))
      Custom.Of(Map(Custom.Common -> None, Custom.Ashkenaz -> Some(torah)))


  object SheminiAtzeresAndSimchasTorahInHolyLand extends WeekdayReading, ShabbosReading:
    final override def weekday(day: Named): Reading = SimchasTorah.weekday(day)

    override def shabbos(day: Named): Reading = weekday(day)

  object Chanukah:
    final def shabbos(
      day: Named,
      roshChodeshDay: Option[Named],
      dayNumber: Int,
      weeklyReading: WeeklyReading
    ): Reading =
      val result = replaceMaftirAndHaftarah(weeklyReading.getMorningReading,
        maftir = fromDay(day, full(dayNumber)),
        haftarah = if dayNumber < 8 then shabbos1Haftarah else shabbos2Haftarah)

      roshChodeshDay.fold(result)(roshChodeshDay => RoshChodesh.addShabbosMaftirAs7thAliyah(result, roshChodeshDay))

    final def weekday(
      day: Named,
      roshChodeshDay: Option[Named],
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

    private def day1Cohen: Torah = torahFor("Chanukah", "day1Cohen")

    private val korbanot: Seq[Fragment] = torahFor("Chanukah", "korbanot").spans

    private val shabbos1Haftarah: Haftarah.Customs = haftarahFor("Chanukah", "shabbos1Haftarah") // = Beha'aloscha Common

    private val shabbos2Haftarah: Haftarah.Customs = haftarahFor("Chanukah", "shabbos2Haftarah")

  sealed trait SpecialParsha:
    final def transform(
      reading: Reading,
      day: Named,
      roshChodeshDay: Option[Named]
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
    override protected val maftir: Maftir = maftirFor("ParshasShekalim", "maftir")

    override protected val haftarah: Haftarah.Customs = haftarahFor("ParshasShekalim", "haftarah")

  object ParshasZachor extends SpecialParsha:
    override protected val maftir: Maftir = maftirFor("ParshasZachor", "maftir")

    override val haftarah: Haftarah.Customs = haftarahFor("ParshasZachor", "haftarah")

  object Purim extends WeekdayReading:
    override def weekday(day: Named): Reading = Reading(fromDay(day, torah))

    val torah: Torah = torahFor("Purim", "torah")

  object ShushanPurim extends WeekdayReading:
    override def weekday(day: Named): Reading = Reading(fromDay(day, Purim.torah))

    def shabbos(day: Named, weeklyReading: WeeklyReading): Reading = replaceMaftirAndHaftarah(
      weeklyReading.getMorningReading,
      maftir = fromDay(day, shabbosMaftir),
      haftarah = ParshasZachor.haftarah
    )

    private val shabbosMaftir: Fragment = Torah.merge(Purim.torah.spans.take(3))

  object ParshasParah extends SpecialParsha:
    override protected val maftir: Maftir = maftirFor("ParshasParah", "maftir")

    override protected val haftarah: Haftarah.Customs = haftarahFor("ParshasParah", "haftarah")

  object ParshasHachodesh extends SpecialParsha:
    override protected val maftir: Maftir = maftirFor("ParshasHachodesh", "maftir")

    override protected val haftarah: Haftarah.Customs = haftarahFor("ParshasHachodesh", "haftarah")

  object ShabbosHagodol:
    def transform(day: Named, isErevPesach: Boolean, reading: Reading): Reading =
      reading.transform[Haftarah](fromDay(day, haftarah),
        (custom: Custom, readingCustom: Reading.ReadingCustom, haftarah: Haftarah) =>
          if (custom == Custom.Chabad) && !isErevPesach then readingCustom
          else readingCustom.replaceHaftarah(haftarah)
      )

    private val haftarah: Haftarah.Customs = haftarahFor("ShabbosHagodol", "haftarah")

  object PesachIntermediate extends ShabbosReading:
    final def weekday(day: Named, isPesachOnChamishi: Boolean, dayNumber: Int): Reading =
      val realDayNumber: Int =
        if isPesachOnChamishi && ((dayNumber == 4) || (dayNumber == 5)) then dayNumber-1 else dayNumber
      Reading(fromDay(day, first5(realDayNumber) :+ shabbosMaftir))

    final override def shabbos(day: Named): Reading = simpleReading(
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

    private val torah3: Torah = torahFor("PesachIntermediate", "torah3")

    private val torah4: Torah = torahFor("PesachIntermediate", "torah4")

    private val torah5: Torah =
      val all = IntermediateShabbos.torah.spans
      Torah.aliyot(
        all(3),         // Exodus 34:1-3
        all(4)+all(5),  // Exodus 34:4-17
        all(6)          // Exodus 34:18-26
      )

    private val torah6: Torah = torahFor("PesachIntermediate", "torah6")

    // Maftir for Pesach Intermediate Shabbos and last two days of Pesach
    val maftirEnd: Maftir = maftirFor("PesachIntermediate", "maftirEnd")
    private def shabbosMaftir: Maftir = maftirEnd

    private val shabbosHaftarah: Haftarah.Customs = haftarahFor("PesachIntermediate", "shabbosHaftarah")

  object Pesach1 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = torahFor("Pesach1", "shabbosTorah")

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(4, 7))

    override val maftir: Maftir = maftirFor("Pesach1", "maftir")

    // Piece 2 below is also Vezos Haberachah/Simchas Torah Teiman part 2.
    override protected val haftarah: Haftarah.Customs = haftarahFor("Pesach1", "haftarah")

  object Pesach2 extends WeekdayReading:
    final override def weekday(day: Named): Reading = simpleReading(
      day,
      torah = torah,
      maftir = maftir,
      haftarah = haftarah
    )

    val torah: Torah = Succos1.weekdayTorah

    private val maftir: Maftir = Pesach1.maftir

    private val haftarah: Haftarah.Customs = haftarahFor("Pesach2", "haftarah")

  object Pesach7 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = torahFor("Pesach7", "shabbosTorah")

    override protected val weekdayTorah: Torah = shabbosTorah.drop(Set(2, 4))

    override val maftir: Maftir = PesachIntermediate.maftirEnd

    override protected val haftarah: Haftarah.Customs = haftarahFor("Pesach7", "haftarah")

  object Pesach8 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = FestivalEnd.shabbosTorah

    override protected val weekdayTorah: Torah = FestivalEnd.weekdayTorah

    override protected val maftir: Maftir = Pesach7.maftir

    override protected val haftarah: Haftarah.Customs = haftarahFor("Pesach8", "haftarah")

  object Shavuos1 extends WeekdayReading:
    final override def weekday(day: Named): Reading = simpleReading(
      day,
      torah = torah,
      maftir = maftir,
      haftarah = haftarah
    )

    private val torah: Torah = torahFor("Shavuos1", "torah")

    val maftir: Maftir = maftirFor("Shavuos1", "maftir")

    private val haftarah: Haftarah.Customs = haftarahFor("Shavuos1", "haftarah")

  object Shavuos2 extends ShabbosAndWeekdayReading:
    override protected val shabbosTorah: Torah = FestivalEnd.shabbosTorah

    override protected val weekdayTorah: Torah = FestivalEnd.weekdayTorah

    override protected def maftir: Maftir = Shavuos1.maftir

    override protected val haftarah: Haftarah.Customs = haftarahFor("Shavuos2", "haftarah")

  object Fast:
    private val afternoonTorahPart1: Fragment = torahFor("Fast", "afternoonTorahPart1").spans.head

    val torah: Torah = Torah.aliyot(
      afternoonTorahPart1,                 // Exodus 32:11-14
      IntermediateShabbos.torah.spans(3),  // Exodus 34:1-3
      IntermediateShabbos.torah.spans(4)   // Exodus 34:4-10
    )

    val defaultAfternoonHaftarah: Haftarah.OptionalCustoms =
      haftarahOptionalFor("Fast", "defaultAfternoonHaftarah")

  sealed trait Fast extends WeekdayReading, AfternoonReading:
    override def afternoon(day: Named): Reading =
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
    final override def weekday(day: Named): Reading = Reading(fromDay(day, Fast.torah))

  object FastOfGedalia extends NonTishaBeAvFast:
    override protected val afternoonHaftarahExceptions: Option[Haftarah.OptionalCustoms] =
      Some(haftarahOptionalFor("FastOfGedalia", "afternoonHaftarahExceptions"))

  object FastOfTeves extends NonTishaBeAvFast

  object FastOfEster extends NonTishaBeAvFast

  object FastOfTammuz extends NonTishaBeAvFast

  object TishaBeAv extends Fast:
    final override def weekday(day: Named): Reading = Reading(
      torah = fromDay(day, torah),
      maftir = None,
      haftarah = fromDay(day, haftarah)
    )

    private val torah: Torah = torahFor("TishaBeAv", "torah")

    private val haftarah: Haftarah.Customs = haftarahFor("TishaBeAv", "haftarah")

    override protected val afternoonHaftarah: Haftarah.OptionalCustoms =
      haftarahOptionalFor("TishaBeAv", "afternoonHaftarah")

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

  private def readingByCutom(day: Named, customs: (Custom, Torah)*): Reading =
    Reading(Custom.Of(customs.map((custom, torah) => (custom, fromDay(day, torah))).toMap))

  private def simpleReading(
    day: Named,
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
    day: Named,
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
