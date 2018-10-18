package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, Metadata, Named, Names, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

import scala.xml.Elem

sealed class SpecialReading(override val name: String, isShabbosAllowed: Boolean = true) extends Named {
  final override def names: Names = SpecialReading.metadatas(this).names

  def getReading(isShabbos: Boolean): Reading = {
    checkIsShabbosAllowed(isShabbos)

    Reading(
      getAliyotSeq(isShabbos),
      getMaftirOpt,
      getHaftarahOpt
    )
  }

  protected final def checkIsShabbosAllowed(isShabbos: Boolean): Unit =
    if (isShabbos) require(isShabbosAllowed)

  protected def getAliyotSeq(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] =
    aliyotSameAs(isShabbos).getAliyot(isShabbos)

  protected def aliyotSameAs(isShabbos: Boolean): SpecialReading = this

  private final def weekdayAliyot: Option[Aliyot] = SpecialReading.metadatas(this).weekdayAliyot

  final def getWeekdayAliyot: Seq[ChumashSpan.BookSpan] = weekdayAliyot.get.getAliyot

  final def shabbosAliyot: Option[Aliyot] = SpecialReading.metadatas(this).shabbosAliyot

  final def getShabbosAliyot: Seq[ChumashSpan.BookSpan] = shabbosAliyot.get.getAliyot

  final def getAliyot(isShabbos: Boolean): Seq[ChumashSpan.BookSpan] =
    if (isShabbos) getShabbosAliyot else getWeekdayAliyot

  protected def getMaftirOpt: Option[ChumashSpan.BookSpan] = Some(getMaftir)

  protected def getMaftir: ChumashSpan.BookSpan = maftirSameAs.maftir.get

  protected def maftirSameAs: SpecialReading = this

  final def maftir: Option[ChumashSpan.BookSpan] = SpecialReading.metadatas(this).maftir

  protected def getHaftarahOpt: Option[Haftarah] = Some(haftarahSameAs.haftarah.get)

  protected def haftarahSameAs: SpecialReading = this

  final def haftarah: Option[Haftarah] = SpecialReading.metadatas(this).haftarah
}

object SpecialReading {
  trait NoHaftarah { self: SpecialReading =>
    protected final override def getMaftirOpt: Option[BookSpan.ChumashSpan.BookSpan] = None
    protected final override def getHaftarahOpt: Option[Haftarah] = None
  }

  case object ShabbosErevRoshChodesh extends SpecialReading("Shabbos Erev Rosh Chodesh")
  case object RoshChodesh extends SpecialReading("Rosh Chodesh")
  case object ShabbosRoshChodeshAdditionalHaftorah extends SpecialReading("Shabbos Rosh Chodesh Additional Haftorah")
  case object ShabbosErevRoshChodeshAdditionalHaftorah extends SpecialReading("Shabbos Erev Rosh Chodesh Additional Haftorah")

  case object Fast extends SpecialReading("Fast", isShabbosAllowed = false) with NoHaftarah {
    protected override def getAliyotSeq(isShabbos: Boolean): Seq[BookSpan.ChumashSpan.BookSpan] =
      Fast.getAliyot(isShabbos) ++ FastPart2.getAliyot(isShabbos)

    def getAfternoonReading: Reading = {
      val aliyot1 = Fast.getWeekdayAliyot
      val aliyot2 = FastPart2.getWeekdayAliyot
      Reading(
        aliyot = aliyot1 ++ aliyot2.init,
        maftir = Some(aliyot2.last),
        haftarah = FastPart2.getHaftarahOpt
      )
    }
  }

  case object FastPart2 extends SpecialReading("Fast Part 2")

  // TODO: Rambam custom - haftara only shacharis;
  //       Sfaradim Eretz Isroel fast haftara (except Zom Gedalia - Shuva Isroel
  case object TishaBeAv extends SpecialReading("Tisha BeAv", isShabbosAllowed = false) {
    protected override def getAliyotSeq(isShabbos: Boolean): Seq[BookSpan.ChumashSpan.BookSpan] =
      getAliyot(false).init

    protected override def getMaftir: BookSpan.ChumashSpan.BookSpan =
      getAliyot(false).last
  }

  case object IntermediateShabbos extends SpecialReading("Intermediate Shabbos")

  case object RoshHashanah1 extends SpecialReading("Rosh Hashanah 1")

  case object RoshHashanah2 extends SpecialReading("Rosh Hashanah 2", isShabbosAllowed = false) {
    protected override def maftirSameAs: SpecialReading = RoshHashanah1
  }

  case object YomKippur extends SpecialReading("Yom Kippur")

  case object YomKippurAfternoon extends SpecialReading("Yom Kippur Afternoon") {
    protected override def getAliyotSeq(isShabbos: Boolean): Seq[BookSpan.ChumashSpan.BookSpan] =
      getWeekdayAliyot.init

    protected override def getMaftir: BookSpan.ChumashSpan.BookSpan =
      getWeekdayAliyot.last
  }

  case object Succos extends SpecialReading("Succos")

  case object Succos2 extends SpecialReading("Succos 2") {
    protected override def aliyotSameAs(isShabbos: Boolean): SpecialReading = Succos
    protected override def maftirSameAs: SpecialReading = Succos
  }

  // TODO Custom rav Nae Eretz Isroel: n day of Sukkos all alios (1 - 4) korbonos of n day.
  case object SuccosIntermediate extends SpecialReading("Succos Intermediate") {
    def getReading(isShabbos: Boolean, number: Int, inHolyLand: Boolean): Reading = {
      val aliyot: Seq[ChumashSpan.BookSpan] = getWeekdayAliyot
      def korbanot(n: Int): ChumashSpan.BookSpan = aliyot(n-1)

      if (number == 6) require(inHolyLand)
      val last: ChumashSpan.BookSpan =
        if (inHolyLand) korbanot(number)
        else ChumashSpan.merge(Seq(korbanot(number), korbanot(number+1)))

      if (isShabbos) Reading(
        // TODO RULE: on Succos Intermediate Shabbos, add aliyot "Intermediate Shabbos".
        aliyot = aliyot,
        maftir = Some(last),
        haftarah = getHaftarahOpt // TODO ?
      ) else {
        val n: Int = if (number <= 4) number else 4
        val first3: Seq[ChumashSpan.BookSpan] = Seq(korbanot(n), korbanot(n+1), korbanot(n+2))
        Reading(first3 :+ last, None, None)
      }
    }
  }

  case object SheminiAtzeres extends SpecialReading("Shemini Atzeres")

  case object SimchasTorah extends SpecialReading("Simchas Torah", isShabbosAllowed = false) {
    protected override def getAliyotSeq(isShabbos: Boolean): Seq[BookSpan.ChumashSpan.BookSpan] =
      getAliyot(isShabbos) ++ SimchasTorahChassanBereishis.getAliyot(isShabbos)

    protected override def maftirSameAs: SpecialReading = SheminiAtzeres
  }

  case object SimchasTorahChassanBereishis extends SpecialReading("Simchas Torah Chassan Bereishis")

  case object Channukah extends SpecialReading("Channukah") {
    // TODO: Minhagim kria Chanuka
    // TODO Mingag Chabad (Ramo) - what is minhag Chabad? Same as Ashkenaz on the 1st day? What Ramo?!
    def getReading(
      isShabbos: Boolean,
      number: Int,
      isRoshChodesh: Boolean,
      weeklyReading: Option[WeeklyReading]): Reading =
    {
      val aliyot: Seq[ChumashSpan.BookSpan] = getWeekdayAliyot
      def first(n: Int): ChumashSpan.BookSpan = aliyot(2*n-1)
      def second(n: Int): ChumashSpan.BookSpan = aliyot(2*n  )
      def split(n: Int): Seq[ChumashSpan.BookSpan] = Seq(first(n), second(n))
      def full(n: Int): ChumashSpan.BookSpan = ChumashSpan.merge(split(n))

      if (isRoshChodesh) require((number == 6) || (number == 7))
      if (number == 6) require(isRoshChodesh)

      if (isShabbos) Reading(
        aliyot = weeklyReading.get.getReading.aliyot,
        maftir = Some(full(number)),
        haftarah = Some((if (number < 8) ChannukahShabbos1 else ChannukahShabbos2).haftarah.get)
      )
//      else  if (isRoshChodesh) {
//      RULE: when a day of Channukah falls on Rosh Chodesh (6th - always; 7th - sometimes),
//      first 3 aliyot are from Rosh Chodesh (see above); aliyah 4: n.1+n.2
//          ???
//        }
      else {
        val result: Custom.Of[Seq[ChumashSpan.BookSpan]] =
          if (number == 1) Map(
              Custom.Ashkenaz -> (aliyot.head +: split(1)),
              Custom.Sefard -> Seq(aliyot.head, full(1), full(2))
          ) else if (number < 8) {
            val common = split(number)
            val ashkenazAndChabad = common :+ full(if (number < 7) number+1 else 1)
            Map(
              Custom.Ashkenaz -> ashkenazAndChabad,
              Custom.Chabad -> ashkenazAndChabad,
              Custom.Sefard -> (common :+ full(number))
            )
          } else {
            val endAliyot = ChannukahEnd.getWeekdayAliyot
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

  case object ChannukahEnd extends SpecialReading("Channukah End")
  case object ChannukahShabbos1 extends SpecialReading("Channukah Shabbos 1")
  case object ChannukahShabbos2 extends SpecialReading("Channukah Shabbos 2")

  // TODO when are they? add in Schedule.
  case object ParshasShekalim extends SpecialReading("Parshas Shekalim")
  case object ParshasZachor extends SpecialReading("Parshas Zachor")
  case object ParshasParah extends SpecialReading("Parshas Parah")
  case object ParshasHachodesh extends SpecialReading("Parshas Hachodesh")

  case object Purim extends SpecialReading("Purim", isShabbosAllowed = false) with NoHaftarah

  case object ShabbosHagodol extends SpecialReading("Shabbos Hagodol") // TODO in Schedule

  case object Pesach extends SpecialReading("Pesach")

  case object Pesach2 extends SpecialReading("Pesach 2", isShabbosAllowed = false) {
    protected override def maftirSameAs: SpecialReading = Pesach
  }

  sealed class PesachIntermediate(name: String, isShabbosAllowed: Boolean = true)
    extends SpecialReading(name, isShabbosAllowed) with NoHaftarah
  {
    override def getReading(isShabbos: Boolean): Reading = {
      checkIsShabbosAllowed(isShabbos)
      if (isShabbos) PesachIntermediateShabbos.getReading(isShabbos) else super.getReading(isShabbos)
    }

    protected override def getAliyotSeq(isShabbos: Boolean): Seq[BookSpan.ChumashSpan.BookSpan] =
      super.getAliyotSeq(isShabbos) :+ Pesach7.getMaftir
  }

  case object Pesach2InHolyLand extends PesachIntermediate("Pesach 2 in Holy Land", isShabbosAllowed = false)
  case object Pesach3 extends PesachIntermediate("Pesach 3")

  case object Pesach4 extends PesachIntermediate("Pesach 4", isShabbosAllowed = false) {
    def getReading(isShabbos: Boolean, isPesachOnChamishi: Boolean): Reading = {
      checkIsShabbosAllowed(isShabbos)
      (if (!isPesachOnChamishi) this else Pesach3).getReading(isShabbos)
    }
  }

  case object Pesach5 extends PesachIntermediate("Pesach 5") {
    def getReading(isShabbos: Boolean, isPesachOnChamishi: Boolean): Reading = {
      checkIsShabbosAllowed(isShabbos)
      (if (isShabbos || !isPesachOnChamishi) this else Pesach4).getReading(isShabbos)
    }
  }

  case object Pesach6 extends PesachIntermediate("Pesach 6", isShabbosAllowed = false)

  case object PesachIntermediateShabbos extends SpecialReading("Pesach Intermediate Shabbos") {
    protected override def aliyotSameAs(isShabbos: Boolean): SpecialReading = IntermediateShabbos
    protected override def maftirSameAs: SpecialReading = Pesach7
  }

  case object Pesach7 extends SpecialReading("Pesach 7")

  case object Pesach8 extends SpecialReading("Pesach 8") {
    protected override def aliyotSameAs(isShabbos: Boolean): SpecialReading =
      if (isShabbos) SheminiAtzeres else this

    protected override def maftirSameAs: SpecialReading = Pesach7
  }

  case object Shavuos extends SpecialReading("Shavuos", isShabbosAllowed = false)

  case object Shavuos2 extends SpecialReading("Shavuos 2") {
    protected override def aliyotSameAs(isShabbos: Boolean): SpecialReading =
      if (isShabbos) SheminiAtzeres else Pesach8

    protected override def maftirSameAs: SpecialReading = Shavuos
  }

  // Needs to be lazy for initialization/metadata loading to work...
  private lazy val values: Seq[SpecialReading] = Seq(
    ShabbosErevRoshChodesh, ShabbosErevRoshChodeshAdditionalHaftorah,
    RoshChodesh, ShabbosRoshChodeshAdditionalHaftorah,
    Fast, FastPart2, TishaBeAv, IntermediateShabbos,

    RoshHashanah1, RoshHashanah2, YomKippur, YomKippurAfternoon,
    Succos, Succos2, SuccosIntermediate,
    SheminiAtzeres, SimchasTorah, SimchasTorahChassanBereishis,
    Channukah, ChannukahEnd, ChannukahShabbos1, ChannukahShabbos2,
    ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh,
    Purim, ShabbosHagodol,
    PesachIntermediateShabbos,
    Pesach, Pesach2InHolyLand, Pesach2, Pesach3,
    Pesach4, Pesach5, Pesach6, Pesach7, Pesach8,
    Shavuos, Shavuos2
  )

  private final case class SpecialReadingMetadata(
    names: Names,
    weekdayAliyot: Option[Aliyot],
    shabbosAliyot: Option[Aliyot],
    maftir: Option[ChumashSpan.BookSpan],
    haftarah: Option[Haftarah]
  )

  private lazy val metadatas: Map[SpecialReading, SpecialReadingMetadata] = Metadata.loadMetadata(
    keys = values,
    obj = SpecialReading.this,
    elementName = "day"
  ).map { case (day, metadata) =>
    metadata.attributes.close()

    val (torahElements, maftriElements, haftarahElements) = XML.span(metadata.elements,
      "torah", "maftir", "haftarah")

    val (weekdayAliyot: Option[Aliyot], shabbosAliyot: Option[Aliyot]) =
      XML.noMoreThanOne(torahElements).fold[(Option[Aliyot], Option[Aliyot])]((None, None))(parseTorah)

    day -> SpecialReadingMetadata(
      metadata.names,
      weekdayAliyot = weekdayAliyot,
      shabbosAliyot = shabbosAliyot,
      maftir = XML.noMoreThanOne(maftriElements).map(parseMaftir),
      haftarah = XML.noMoreThanOne(haftarahElements).map(parseHaftarah)
    )
  }

  private def parseTorah(element: Elem): (Option[Aliyot], Option[Aliyot]) = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: BookSpan.ChumashSpan.BookSpan = BookSpan.ChumashSpan.parse(attributes).resolve
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
    require(span.to.isEmpty)
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
