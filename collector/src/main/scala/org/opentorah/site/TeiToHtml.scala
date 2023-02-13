package org.opentorah.site

import org.opentorah.calendar.Calendar
import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.roman.{Gregorian, Julian}
import org.opentorah.metadata.Language
import org.opentorah.tei.{Date, EntityName, EntityType, Pb, Tei}
import org.opentorah.util.Files
import org.opentorah.xml.{A, Attribute, Element, Html, Namespace, ScalaXml, Xml}
import zio.{URIO, ZIO}

import java.net.URI

// Converting TEI to HTML
object TeiToHtml:
  // TODO handle table/tr/td systematically

  private def isFootnote(element: ScalaXml.Element): Boolean =
    (ScalaXml.getName(element) == "note") && placeAttribute.get(ScalaXml)(element).contains("end")

  private def isFootnotesContainer(element: ScalaXml.Element): Boolean = false

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
  private def elementToHtml(element: ScalaXml.Element): zio.URIO[LinksResolver, ScalaXml.Element] =
    val children: ScalaXml.Nodes = ScalaXml.getChildren(element)

    ScalaXml.getName(element) match
      case label if EntityType.isName(label) =>
        require(!ScalaXml.isEmpty(children), element)
        val ref: Option[String] = EntityName.refAttribute.get(ScalaXml)(element)

        if ref.isEmpty then ZIO.succeed(TeiToHtml.namespace(A.empty(children))) else
          ZIO.environmentWithZIO[LinksResolver](_.get.findByRef(ref.get))
            .map(_.getOrElse(A(ref.toSeq))(children))
            .map(TeiToHtml.namespace)

      case "ref" =>
        require(!ScalaXml.isEmpty(children), element)
        reference(element).map(_(children))
          .map(TeiToHtml.namespace)

      case "ptr" =>
        require(ScalaXml.isEmpty(children), element)
        reference(element).map(_(Seq.empty))
          .map(TeiToHtml.namespace)

      // TODO feed pageId through State to obtain unique id
      case Pb.elementName =>
        require(ScalaXml.isEmpty(children), element)
        val pageId: String = Pb.pageId(Pb.nAttribute.get(ScalaXml)(element))
        ZIO.environmentWithZIO[LinksResolver](_.get.facs(pageId)).map(_
          .getOrElse(A(Seq(pageId)))
          .setId(pageId)
          (text = facsimileSymbol)
        )
          .map(TeiToHtml.namespace)

      case "graphic" =>
        // Note: in TEI <graphic> can contain <desc>, but we are treating it as empty.
        require(ScalaXml.isEmpty(children), element)
        ZIO.succeed(TeiToHtml.namespace(<img src={urlAttribute.get(ScalaXml)(element)}/>))

      //      case "table" => ZIO.succeed(Html.table(children))
      // Note: before the first row there can be <head>HEAD</head>;
      // it should become <caption>transform(HEAD)</caption>.
      case "row" => ZIO.succeed(TeiToHtml.tr(children))
      case "cell" => ZIO.succeed(TeiToHtml.td(colsAttribute.get(ScalaXml)(element), children))
      case "date" => ZIO.succeed(TeiToHtml.addTooltip(dateTooltip(element), element))
      case "gap" => ZIO.succeed(TeiToHtml.addTooltip(gapTooltip(element), element))
      case _ => ZIO.succeed(element)

  private def reference(element: ScalaXml.Element): zio.URIO[LinksResolver, A] =
    val uri: URI = URI(targetAttribute.get(ScalaXml)(element))

    // TODO maybe just call up regardless?
    if uri.isAbsolute then ZIO.succeed(A(uri))
    else ZIO.environmentWithZIO[LinksResolver](_.get.resolve(Files.splitUrl(uri.getPath))).map(_
      .map(a => Option(uri.getFragment).fold(a)(a.setFragment))
      .getOrElse(A(uri))
    )

  private def gapTooltip(element: ScalaXml.Element): ScalaXml.Nodes =
    reasonAttribute.get(ScalaXml)(element).fold(Seq.empty)(ScalaXml.mkText)

  // TODO move into Calendar
  private final class DateInterval(
    val from: Calendar#Day,
    val to: Calendar#Day
  ) {
    def to(calendar: Calendar): DateInterval = DateInterval(
      from.to(calendar),
      to.to(calendar)
    )
  }

  private def parse(when: String, calendar: Calendar): Either[DateInterval, Calendar#Day] =
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
      val toNumbersEffective: Seq[Int] = fromNumbers.take(fromNumbers.length - toNumbers.length) ++ toNumbers

      Left(DateInterval(from(fromNumbers), to(toNumbersEffective)))
    else
      val numbers: Seq[Int] = parseNumbers(when)
      if numbers.length == 3 then Right(from(numbers)) else Left(DateInterval(from(numbers), to(numbers)))

  private def dateTooltip(element: ScalaXml.Element): ScalaXml.Nodes =
    val when: String = Date.whenAttribute.get(ScalaXml)(element)

    try
      val defaultCalendarIsJulian: Boolean = true // TODO get default from content
      val calendarName: Option[String] = Date.calendarAttribute.get(ScalaXml)(element)
      val useJulian: Boolean = calendarName.fold(defaultCalendarIsJulian)(_ == "#julian")
      val calendar: Calendar = if useJulian then Julian else Gregorian

      val displayLanguage: Language.Spec = Language.Russian.toSpec // TODO get from context

      def display(date: Calendar#Day): ScalaXml.Element =
        <td>
          {date.toLanguageString(using displayLanguage)}
        </td>

      def displayInterval(interval: DateInterval): Seq[ScalaXml.Element] =
        Seq(display(interval.from), display(interval.to))

      parse(when, calendar) match {
        case Right(date) =>
          <table xmlns={Html.namespace.uri}>
            <tr>
              <th>Calendar</th> <th>Date</th>
            </tr>{ScalaXml.conditional(useJulian)(
            <tr>
              <td>Julian</td>{display(date)}
            </tr>)}<tr>
            <td>Gregorian</td>{display(if useJulian then date.to(Gregorian) else date)}
          </tr>
            <tr>
              <td>Jewish</td>{display(date.to(Jewish).asInstanceOf[Calendar#Day] /* TODO WTF?! */)}
            </tr>
          </table>

        case Left(interval) =>
          <table xmlns={Html.namespace.uri}>
            <tr>
              <th>Calendar</th> <th>From</th> <th>To</th>
            </tr>{ScalaXml.conditional(useJulian)(
            <tr>
              <td>Julian</td>{displayInterval(interval)}
            </tr>)}<tr>
            <td>Gregorian</td>{displayInterval(if useJulian then interval.to(Gregorian) else interval)}
          </tr>
            <tr>
              <td>Jewish</td>{displayInterval(interval.to(Jewish))}
            </tr>
          </table>
      }

    catch
      case e => throw IllegalArgumentException(s"Exception processing when=$when", e)


  private def isInNamespace(element: ScalaXml.Element): Boolean =
    ScalaXml.getNamespace(element) == Tei.namespace.default

  private def addPrefix(name: String): String = s"${Html.namespace.getPrefix.get}-$name"

  def toHtml(element: ScalaXml.Element): URIO[LinksResolver, ScalaXml.Element] =
    for
      footnotesDone <- processFootnotes(element).provideSomeLayer[LinksResolver](Footnotes.empty)
      html <- elementsToHtml(footnotesDone)
    yield
      html

  private def processFootnotes(element: ScalaXml.Element): URIO[Footnotes & LinksResolver, ScalaXml.Element] = for
    isEmpty: Boolean <- Footnotes.isEmpty
    doPush: Boolean = isEmpty || (isInNamespace(element) && isFootnotesContainer(element))
    _ <- if !doPush then ZIO.succeed(()) else Footnotes.push
    newElement: ScalaXml.Element <-
      if isInNamespace(element) && isFootnote(element)
      then Footnotes.footnote(element)
      else transformChildren(processFootnotes, element)
    result: ScalaXml.Element <-
      if !doPush
      then ZIO.succeed(newElement)
      else processLevels(newElement, Seq.empty)
  yield result

  private def processLevels(
    newElement: ScalaXml.Element,
    levels: Seq[Seq[ScalaXml.Element]]
  ): URIO[Footnotes & LinksResolver, ScalaXml.Element] = for
    footnotes: Seq[ScalaXml.Element] <- Footnotes.get
    result: ScalaXml.Element <-
      if footnotes.isEmpty then for
        _ <- Footnotes.pop
      yield
        if levels.isEmpty
        then newElement
        else ScalaXml.appendChildren(newElement,
          (for (level, depth) <- levels.zipWithIndex yield TeiToHtml.footnoteLevel(level, depth)).flatten
        )
      else for
        nextLevel: Seq[ScalaXml.Element] <- ZIO.foreach(footnotes)(processFootnotes)
        result <- processLevels(newElement, levels :+ nextLevel)
      yield result
  yield result

  private def elementsToHtml(oldElement: ScalaXml.Element): URIO[LinksResolver, ScalaXml.Element] = for
    element: ScalaXml.Element <- transformChildren(elementsToHtml, oldElement)
    result: ScalaXml.Element <- if !isInNamespace(element) then ZIO.succeed(element) else

      val attributes: Attribute.StringValues =
        for attribute: Attribute.Value[String] <- ScalaXml.getAttributes(element) yield
          TeiToHtml.xml2htmlAttribute.get(attribute.attribute).map(_.optional.withValue(attribute.value)).getOrElse {
            val name: String = attribute.attribute.name
            if !TeiToHtml.reservedAttributes.contains(name) then attribute
            else Attribute(addPrefix(name)).optional.withValue(attribute.value)
          }

      for newElement: ScalaXml.Element <- elementToHtml(element) yield
        if isInNamespace(newElement) then
          val name: String = ScalaXml.getName(newElement)
          ScalaXml.setAttributes(
            element = if !TeiToHtml.reservedElements.contains(name) then newElement else ScalaXml.rename(newElement, addPrefix(name)),
            attributes = attributes
          )
        else
          ScalaXml.addAttributes(
            element = newElement,
            attributes = Html.classAttribute.required.withValue(ScalaXml.getName(element)) +: attributes
          )

  yield result

  private def transformChildren[T](
    transform: ScalaXml.Element => URIO[T, ScalaXml.Element],
    element: ScalaXml.Element
  ): URIO[T, ScalaXml.Element] = for
    children: ScalaXml.Nodes <- ZIO.foreach(ScalaXml.getChildren(element))((node: ScalaXml.Node) =>
      if !ScalaXml.isElement(node) then ZIO.succeed(node) else transform(ScalaXml.asElement(node))
    )
  yield ScalaXml.setChildren(element, children)

  private val xml2htmlAttribute: Map[Attribute[String], Attribute[String]] = Map(
    Xml.idAttribute   -> Html.idAttribute,
    Xml.langAttribute -> Html.langAttribute
    // TODO xml:base? xml:space?
  )

  // TODO eliminate
  def namespace(element: ScalaXml.Element): ScalaXml.Element =
    ScalaXml.declareNamespace(Html.namespace.default, element)

  /*
  I tried to define CSS namespaces like this:
    @namespace tei   url("http://www.tei-c.org/ns/1.0");
    @namespace db    url("http://docbook.org/ns/docbook");
    @namespace xhtml url("http://www.w3.org/1999/xhtml");
  and use them in CSS rules like this: tei|div, docbook|title.

  It seems that in browser DOM all elements are in the HTML5 xhtml namespace
  unless xmlns attribute is present on that element;
  why are the namespace declarations not inherited is not clear.

  So, I prefix the names of the elements from non-HTML namespaces with the namespace prefix
  if their names clash with the HTML namespace in a way that makes CSS styling difficult.
  For instance, I use <div> to structure the layout, but need to be able to style TEI
  depending on the level of nesting of TEI divs.
  Also, HTML disallows tables within paragraphs, so to have a tooltip inside a TEI paragraph,
  it needs to not be an HTML <p> (and of course, namespace is ignored...)
  */
  private val reservedElements: Set[String] = Set("head", "body", "title", "div", "p")

  private val reservedAttributes: Set[String] = Set("class", "target", "lang", "frame")

  private def tooltip(content: ScalaXml.Nodes): ScalaXml.Element =
    <span xmlns={Html.namespace.uri} class="tooltip">
      {content}
    </span>

  private def addTooltip(content: ScalaXml.Nodes, element: ScalaXml.Element): ScalaXml.Element =
    ScalaXml.prependChildren(element, tooltip(content))

  def footnote(contentId: String, srcId: String, symbol: String, content: ScalaXml.Nodes): ScalaXml.Element =
    <span xmlns={Html.namespace.uri} class="footnote" id={contentId}>
      <a href={s"#$srcId"} class="footnote-backlink">
        {symbol}
      </a>{content}
    </span>

  def footnoteRef(contentId: String, srcId: String, symbol: String): ScalaXml.Element =
    <a xmlns={Html.namespace.uri} href={s"#$contentId"} class="footnote-link" id={srcId}>
      {symbol}
    </a>

  private def footnoteLevel(content: Seq[ScalaXml.Element], depth: Int): ScalaXml.Nodes =
      <hr class="footnotes-line"/> ++
      <div xmlns={Html.namespace.uri} class="footnotes">
        {content}
      </div>

  def table(children: ScalaXml.Nodes): ScalaXml.Element =
    <table xmlns={Html.namespace.uri}>
      {children}
    </table>

  def tr(children: ScalaXml.Nodes): ScalaXml.Element =
    <tr xmlns={Html.namespace.uri}>
      {children}
    </tr>

  def td(colspan: Option[String], children: ScalaXml.Nodes): ScalaXml.Element =
    <td xmlns={Html.namespace.uri} colspan={colspan.orNull}>
      {children}
    </td>
