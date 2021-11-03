package org.opentorah.tei

import org.opentorah.calendar.Calendar
import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.roman.{Gregorian, Julian}
import org.opentorah.html.{a, ToHtml}
import org.opentorah.metadata.Language
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Dialect, Element, Namespace, Parsable, Parser, PrettyPrinter, ScalaXml, Unparser}
import zio.{Has, URIO}
import java.net.URI

final class Tei(
  val teiHeader: TeiHeader,
  val text: Text
):
  def copy(
    teiHeader: TeiHeader = teiHeader,
    text: Text = text
  ): Tei = Tei(
    teiHeader,
    text
  )

object Tei extends Element[Tei]("TEI"), Dialect, ToHtml[Has[LinksResolver]]:

  override val namespace: Namespace = Namespace(uri = "http://www.tei-c.org/ns/1.0", prefix="tei")

  override val mimeType: String = "application/tei+xml"

  /////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>\n"""
  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )

  override def contentParsable: Parsable[Tei] = new Parsable[Tei]:
    override lazy val parser: Parser[Tei] = for
      teiHeader: TeiHeader <- TeiHeader.required()
      text: Text <- Text.required()
    yield Tei(
      teiHeader,
      text
    )

    override lazy val unparser: Unparser[Tei] = concat(
      TeiHeader.required(_.teiHeader),
      Text.required(_.text)
    )

  def concat[A](unparsers: Unparser[A]*): Unparser[A] =
    Unparser.concatInNamespace(Tei.namespace, unparsers)

  override protected def isFootnote(element: ScalaXml.Element): Boolean =
    (ScalaXml.getName(element) == "note") && placeAttribute.get(ScalaXml)(element).contains("end")

  private val targetAttribute: Attribute.Required[String] = Attribute("target").required
  private val urlAttribute: Attribute.Required[String] = Attribute("url").required
  private val placeAttribute: Attribute.Optional[String] = Attribute("place").optional
  private val colsAttribute: Attribute.Optional[String] = Attribute("cols").optional
  private val reasonAttribute: Attribute.Optional[String] = Attribute("reason").optional

  // TODO move to Site.Navigation somehow?
  val facsimileSymbol: String = "⎙"

  // Note: it is possible to add the tokens from the 'rendition' attribute to the value of the HTML
  // class attribute by augmenting Html.To - but I do not see the need: CSS styling can be applied
  // based on the 'rendition' itself.
  // TEI allows for in-element styling using attribute `style` - and browsers apply CSS from there too!
  override protected def elementToHtml(
    element: ScalaXml.Element
  ): URIO[Has[LinksResolver], (ScalaXml.Element, ScalaXml.Nodes)] =
    val children: ScalaXml.Nodes = ScalaXml.getChildren(element)

    ScalaXml.getName(element) match
      case label if EntityType.isName(label) =>
        require(!ScalaXml.isEmpty(children), element)
        val ref: Option[String] = EntityName.refAttribute.get(ScalaXml)(element)

        if ref.isEmpty then succeed(a.empty(children)) else
          URIO.accessM[Has[LinksResolver]](_.get.findByRef(ref.get)).map(_.
            getOrElse(a(ref.toSeq))
            (children)
          ).map(noTooltip)

      case "ref" =>
        require(!ScalaXml.isEmpty(children), element)
        reference(element).map(_(children)).map(noTooltip)

      case "ptr" =>
        require(ScalaXml.isEmpty(children), element)
        reference(element).map(_(Seq.empty)).map(noTooltip)

      // TODO feed pageId through State to obtain unique id
      case Pb.elementName =>
        require(ScalaXml.isEmpty(children), element)
        val pageId: String = Pb.pageId(Pb.nAttribute.get(ScalaXml)(element))
        URIO.accessM[Has[LinksResolver]](_.get.facs(pageId)).map(_
          .getOrElse(a(Seq(pageId)))
          .setId(pageId)
          (text = facsimileSymbol)
        ).map(noTooltip)

      case "graphic" =>
        // Note: in TEI <graphic> can contain <desc>, but we are treating it as empty.
        require(ScalaXml.isEmpty(children), element)
        succeed(<img src={urlAttribute.get(ScalaXml)(element)}/>)

//      case "table" =>
//        URIO.succeed(noTooltip(<table>{children}</table>))

      // Note: before the first row there can be <head>HEAD</head>;
      // it should become <caption>transform(HEAD)</caption>.
      case "row" =>
        succeed(<tr>{children}</tr>)

      case "cell" =>
        succeed(<td colspan={colsAttribute.get(ScalaXml)(element).orNull}>{children}</td>)

      case "date" =>
        URIO.succeed((element, dateTooltip(element)))

      case "gap" =>
        URIO.succeed((element, gapTooltip(element)))

      case _ =>
        succeed(element)

  private def reference(element: ScalaXml.Element): URIO[Has[LinksResolver], a] =
    val uri: URI = URI(targetAttribute.get(ScalaXml)(element))

    // TODO maybe just call up regardless?
    if uri.isAbsolute then URIO.succeed(a(uri))
    else URIO.accessM[Has[LinksResolver]](_.get.resolve(Files.splitUrl(uri.getPath))).map(_
      .map(a => Option(uri.getFragment).fold(a)(a.setFragment))
      .getOrElse(a(uri))
    )

  private def gapTooltip(element: ScalaXml.Element): ScalaXml.Nodes =
    reasonAttribute.get(ScalaXml)(element).fold(Seq.empty)(ScalaXml.mkText)

  // TODO move into Calendar - but under different name...
  private final class Interval(
    val from: Calendar#Day,
    val to: Calendar#Day
  ) {
    def to(calendar: Calendar): Interval = Interval(
      from.to(calendar),
      to.to(calendar)
    )
  }

  private def parse(when: String, calendar: Calendar): Either[Interval, Calendar#Day] =
    def parseNumbers(when: String): Seq[Int] =
      val result: Seq[Int] = when.split("-").map(_.toInt).toIndexedSeq
      require(result.length >= 1, s"Too few dashes in 'when': $when")
      require(result.length <= 3, s"Too many dashes in 'when': $when")
      result

    def from(numbers: Seq[Int]): Calendar#Day =
      val year: Calendar#Year = calendar.Year(numbers(0))
      if numbers.length == 1 then year.firstDay else
        val month: Calendar#Month = year.month(numbers(1))
        if numbers.length == 2 then month.firstDay else
          month.day(numbers(2))

    def to(numbers: Seq[Int]): Calendar#Day =
      val year: Calendar#Year = calendar.Year(numbers(0))
      if numbers.length == 1 then year.lastDay else
        val month: Calendar#Month = year.month(numbers(1))
        if numbers.length == 2 then month.lastDay else
          month.day(numbers(2))

    if when.contains("..") then
      // quoting dots to stop regex compiler from processing them specially
      val parts: Seq[String] = when.split("\\.\\.").toIndexedSeq
      require(parts.length == 2, s"Bad explicit interval: $when")
      val fromNumbers: Seq[Int] = parseNumbers(parts(0))
      val toNumbers: Seq[Int] = parseNumbers(parts(1))
      require(toNumbers.length <= fromNumbers.length, s"Too many dashes in the 'to': $when")
      val toNumbersEffective: Seq[Int] = fromNumbers.take(fromNumbers.length-toNumbers.length) ++ toNumbers

      Left(Interval(from(fromNumbers), to(toNumbersEffective)))
    else
      val numbers: Seq[Int] = parseNumbers(when)
      if numbers.length == 3 then Right(from(numbers)) else Left(Interval(from(numbers), to(numbers)))

  def dateTooltip(element: ScalaXml.Element): ScalaXml.Nodes =
    val when: String = Date.whenAttribute.get(ScalaXml)(element)

    try
      val defaultCalendarIsJulian: Boolean = true // TODO get default from content
      val calendarName: Option[String] = Date.calendarAttribute.get(ScalaXml)(element)
      val useJulian: Boolean = calendarName.fold(defaultCalendarIsJulian)(_ == "#julian")
      val calendar: Calendar = if useJulian then Julian else Gregorian

      val displayLanguage: Language.Spec = Language.Russian.toSpec // TODO get from context
      def display(date: Calendar#Day): ScalaXml.Element =
        <td>{date.toLanguageString(using displayLanguage)}</td>

      def displayInterval(interval: Interval): Seq[ScalaXml.Element] =
        Seq(display(interval.from), display(interval.to))

      parse(when, calendar) match {
        case Right(date) =>
          <table>
            <tr><th>Calendar</th><th>Date</th></tr>
            {ScalaXml.conditional(useJulian)(
            <tr><td>Julian   </td>{display(date)}</tr>)}
            <tr><td>Gregorian</td>{display(if useJulian then date.to(Gregorian) else date)}</tr>
            <tr><td>Jewish   </td>{display(date.to(Jewish).asInstanceOf[Calendar#Day] /* TODO WTF?! */)}</tr>
          </table>

        case Left(interval) =>
          <table>
            <tr><th>Calendar</th><th>From</th><th>To</th></tr>
            {ScalaXml.conditional(useJulian)(
            <tr><td>Julian   </td>{displayInterval(interval)}</tr>)}
            <tr><td>Gregorian</td>{displayInterval(if useJulian then interval.to(Gregorian) else interval)}</tr>
            <tr><td>Jewish   </td>{displayInterval(interval.to(Jewish))}</tr>
          </table>
      }

    catch
      case e => throw IllegalArgumentException(s"Exception processing when=$when", e)
