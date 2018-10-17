package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, Named, Names, Metadata, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

import scala.xml.Elem

sealed class SpecialReading(override val name: String) extends Named {
  final override def names: Names = SpecialReading.metadatas(this).names

  final def weekdayAliyot: Option[Aliyot] = SpecialReading.metadatas(this).weekdayAliyot

  final def shabbosAliyot: Option[Aliyot] = SpecialReading.metadatas(this).shabbosAliyot

  final def getAliyot(isShabbos: Boolean): Option[Aliyot] = if (isShabbos) shabbosAliyot else weekdayAliyot

  final def maftir: Option[ChumashSpan.BookSpan] = SpecialReading.metadatas(this).maftir

  final def haftarah: Option[Haftarah] = SpecialReading.metadatas(this).haftarah
}

object SpecialReading {

  trait Simple {
    def getReading(isShabbos: Boolean): Reading
  }

  case object ShabbosErevRoshChodesh extends SpecialReading("Shabbos Erev Rosh Chodesh")
  case object RoshChodesh extends SpecialReading("Rosh Chodesh")
  case object ShabbosRoshChodeshAdditionalHaftorah extends SpecialReading("Shabbos Rosh Chodesh Additional Haftorah")
  case object ShabbosErevRoshChodeshAdditionalHaftorah extends SpecialReading("Shabbos Erev Rosh Chodesh Additional Haftorah")

  case object Fast extends SpecialReading("Fast") with Simple {
    override def getReading(isShabbos: Boolean): Reading = {
      require(!isShabbos)
      val aliyot1 = Fast.weekdayAliyot.get.getAliyot
      val aliyot2 = FastPart2.weekdayAliyot.get.getAliyot
      Reading(aliyot1 ++ aliyot2)
    }

    def getAfternoonReading: Reading = {
      val aliyot1 = Fast.weekdayAliyot.get.getAliyot
      val aliyot2 = FastPart2.weekdayAliyot.get.getAliyot
      Reading(
        aliyot = aliyot1 ++ aliyot2.init,
        maftir = aliyot2.last,
        haftarah = FastPart2
      )
    }
  }

  case object FastPart2 extends SpecialReading("Fast Part 2")

  // TODO: Rambam custom - haftara only shacharis;
  //       Sfaradim Eretz Isroel fast haftara (except Zom Gedalia - Shuva Isroel
  case object TishaBeAv extends SpecialReading("Tisha BeAv") with Simple {
    override def getReading(isShabbos: Boolean): Reading = {
      require(!isShabbos)
      val aliyot = getAliyot(isShabbos).get.getAliyot
      Reading(
        aliyot = aliyot.init,
        maftir = aliyot.last,
        haftarah = this
      )
    }
  }

  case object ShabbosCholHamoed extends SpecialReading("Shabbos Chol Hamoed")

  case object RoshHashanah1 extends SpecialReading("Rosh Hashanah 1") with Simple {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = this,
      isShabbos = isShabbos,
      maftir = this,
      haftarah = this
    )
  }

  case object RoshHashanah2 extends SpecialReading("Rosh Hashanah 2") with Simple {
    override def getReading(isShabbos: Boolean): Reading = {
      require(!isShabbos)
      Reading(
        aliyot = this,
        isShabbos = isShabbos,
        maftir = RoshHashanah1,
        haftarah = this
      )
    }
  }

  case object YomKippur extends SpecialReading("Yom Kippur") with Simple {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = this,
      isShabbos = false,
      maftir = this,
      haftarah = this
    )
  }

  case object YomKippurAfternoon extends SpecialReading("Yom Kippur Afternoon") {
    def getReading: Reading = {
      val aliyot: Seq[ChumashSpan.BookSpan] = weekdayAliyot.get.getAliyot
      Reading(
        aliyot = aliyot.init,
        maftir = aliyot.last,
        haftarah = this
      )
    }
  }

  case object Succos extends SpecialReading("Succos") with Simple {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = this,
      isShabbos = isShabbos,
      maftir = this,
      haftarah = this
    )
  }

  case object Succos2 extends SpecialReading("Succos 2") with Simple {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = Succos,
      isShabbos = isShabbos,
      maftir = Succos,
      haftarah = this
    )
  }

  // TODO Custom rav Nae Eretz Isroel: n day of Sukkos all alios (1 - 4) korbonos of n day.
  case object SuccosIntermediate extends SpecialReading("Succos Intermediate") {
    def getReading(isShabbos: Boolean, number: Int, inHolyLand: Boolean): Reading = {
      val aliyot: Seq[ChumashSpan.BookSpan] = weekdayAliyot.get.getAliyot
      def korbanot(n: Int): ChumashSpan.BookSpan = aliyot(n-1)

      if (number == 6) require(inHolyLand)
      val last: ChumashSpan.BookSpan =
        if (inHolyLand) korbanot(number)
        else ChumashSpan.merge(Seq(korbanot(number), korbanot(number+1)))

      if (isShabbos) Reading(
        // TODO RULE: on Shabbos Chol Hamoed Succos, add aliyot "Shabbos Chol Hamoed".
        aliyot = aliyot,
        maftir = last,
        haftarah = this // TODO ?
      ) else {
        val n: Int = if (number <= 4) number else 4
        val first3: Seq[ChumashSpan.BookSpan] = Seq(korbanot(n), korbanot(n+1), korbanot(n+2))
        Reading(first3 :+ last)
      }
    }
  }

  case object SheminiAtzeres extends SpecialReading("Shemini Atzeres") with Simple {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = this,
      isShabbos = isShabbos,
      maftir = this,
      haftarah = this
    )
  }

  case object SimchasTorah extends SpecialReading("Simchas Torah") with Simple {
    override def getReading(isShabbos: Boolean): Reading = {
      require(!isShabbos)
      val aliyot = getAliyot(isShabbos).get.getAliyot ++ SimchasTorahChassanBereishis.weekdayAliyot.get.getAliyot
      Reading(
        aliyot = aliyot,
        maftir = SheminiAtzeres,
        haftarah = this
      )
    }
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
      val aliyot: Seq[ChumashSpan.BookSpan] = weekdayAliyot.get.getAliyot
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
            val endAliyot = ChannukahEnd.weekdayAliyot.get.getAliyot
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

  case object Purim extends SpecialReading("Purim") with Simple {
    override def getReading(isShabbos: Boolean): Reading = {
      require(!isShabbos)
      Reading(getAliyot(isShabbos).get.getAliyot)
    }
  }

  case object ShabbosHagodol extends SpecialReading("Shabbos Hagodol") // TODO in Schedule

  case object Pesach extends SpecialReading("Pesach") with Simple {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = this,
      isShabbos = isShabbos,
      maftir = this,
      haftarah = this
    )
  }

  case object Pesach2 extends SpecialReading("Pesach 2") with Simple {
    override def getReading(isShabbos: Boolean): Reading = {
      require(!isShabbos)
      Reading(
        aliyot = this,
        isShabbos = isShabbos,
        maftir = Pesach,
        haftarah = this
      )
    }
  }

  case object Pesach2InHolyLand extends SpecialReading("Pesach 2 in Holy Land") with Simple {
    override def getReading(isShabbos: Boolean): Reading = {
      require(!isShabbos)
      Reading(getAliyot(isShabbos).get.getAliyot :+ Pesach7.maftir.get)
    }
  }

  case object Pesach3 extends SpecialReading("Pesach 3") with Simple {
    override def getReading(isShabbos: Boolean): Reading =
      if (isShabbos) PesachShabbosCholHamoed.getReading
      else Reading(getAliyot(isShabbos).get.getAliyot :+ Pesach7.maftir.get)
  }

  case object Pesach4 extends SpecialReading("Pesach 4") {
    def getReading(isShabbos: Boolean, isPesachOnChamishi: Boolean): Reading = {
      require(!isShabbos)
      Reading((if (isPesachOnChamishi) Pesach3 else this).getAliyot(isShabbos).get.getAliyot :+ Pesach7.maftir.get)
    }
  }

  case object Pesach5 extends SpecialReading("Pesach 5") {
    def getReading(isShabbos: Boolean, isPesachOnChamishi: Boolean): Reading = {
      if (isShabbos) PesachShabbosCholHamoed.getReading
      else Reading((if (isPesachOnChamishi) Pesach4 else this).getAliyot(isShabbos).get.getAliyot :+ Pesach7.maftir.get)
    }
  }

  case object Pesach6 extends SpecialReading("Pesach 6") with Simple {
    override def getReading(isShabbos: Boolean): Reading = {
      require(!isShabbos)
      Reading(getAliyot(isShabbos).get.getAliyot :+ Pesach7.maftir.get)
    }
  }

  case object PesachShabbosCholHamoed extends SpecialReading("Pesach Shabbos Chol Hamoed") {
    def getReading: Reading = Reading(
      aliyot = PesachShabbosCholHamoed.getAliyot(true).get.getAliyot,
      maftir = Pesach7,
      haftarah = this
    )
  }

  case object Pesach7 extends SpecialReading("Pesach 7") with Simple {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = this,
      isShabbos = isShabbos,
      maftir = this,
      haftarah = this
    )
  }

  case object Pesach8 extends SpecialReading("Pesach 8") with Simple {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = (if (isShabbos) SheminiAtzeres.getAliyot(isShabbos) else getAliyot(isShabbos)).get.getAliyot,
      maftir = Pesach7,
      haftarah = this
    )
  }

  case object Shavuos extends SpecialReading("Shavuos") with Simple {
    override def getReading(isShabbos: Boolean): Reading = {
      require(!isShabbos)
      Reading(
        aliyot = this,
        isShabbos = isShabbos,
        maftir = this,
        haftarah = this
      )
    }
  }

  case object Shavuos2 extends SpecialReading("Shavuos 2") with Simple {
    override def getReading(isShabbos: Boolean): Reading = Reading(
      aliyot = (if (isShabbos) SheminiAtzeres.shabbosAliyot else Pesach8.weekdayAliyot).get.getAliyot,
      maftir = Shavuos,
      haftarah = this
    )
  }

  val values: Seq[SpecialReading] = Seq(
    ShabbosErevRoshChodesh, ShabbosErevRoshChodeshAdditionalHaftorah,
    RoshChodesh, ShabbosRoshChodeshAdditionalHaftorah,
    Fast, FastPart2, TishaBeAv, ShabbosCholHamoed,
    RoshHashanah1, RoshHashanah2, YomKippur, YomKippurAfternoon,
    Succos, Succos2, SuccosIntermediate,
    SheminiAtzeres, SimchasTorah, SimchasTorahChassanBereishis,
    Channukah, ChannukahEnd, ChannukahShabbos1, ChannukahShabbos2,
    ParshasShekalim, ParshasZachor, ParshasParah, ParshasHachodesh,
    Purim, ShabbosHagodol,
    PesachShabbosCholHamoed,
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
