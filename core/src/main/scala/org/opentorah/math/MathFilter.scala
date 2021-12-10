package org.opentorah.math

import org.opentorah.docbook.DocBook
import org.opentorah.xml.{Attribute, Dom, Namespace, Sax}
import org.slf4j.{Logger, LoggerFactory}
import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl

final class MathFilter(
  allDelimiters: Seq[DelimitersAndInput],
  processEscapes: Boolean
) extends Sax.WarningFilter:
  private var elementsStack: List[String] = List.empty
  private def pushElement(localName: String): Unit = elementsStack = elementsStack :+ localName
  private def popElement(): Unit = elementsStack = elementsStack.init

  private def currentElement: String = elementsStack.last
  // do not generate DocBook math wrapper if we are inside one of those
  private def currentlyInEquationElement: Boolean = DocBook.equationElements.contains(currentElement)

  private var delimiters: Option[DelimitersAndInput] = None

  private var math: String = ""
  private def addToMath(what: String): Unit = math = math + what

  private def sendToParent(what: String): Unit = super.characters(what.toCharArray, 0, what.length)

  private def element(
    namespace: Namespace,
    localName: String,
    atts: Attributes = new AttributesImpl
  )(content: => Unit): Unit =
    val uri = namespace.uri
    val qName = namespace.qName(localName)

    super.startElement(uri, localName, qName, atts)
    pushElement(localName)

    content

    super.endElement(uri, localName, qName)
    popElement()

  private def prefixMapping(namespace: Namespace)(content: => Unit): Unit =
    val prefix: String = namespace.getPrefix.getOrElse("")

    super.startPrefixMapping(prefix, namespace.uri)
    content
    super.endPrefixMapping(prefix)

  override def startDocument(): Unit =
    flush()

    super.startDocument()

  override def endDocument(): Unit =
    flush()

    super.endDocument()

  override def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit =
    flush()

    val isMathMLMath: Boolean = (uri == MathML.namespace.uri) && (localName == MathML.math)
    val attributes: Attributes = if !isMathMLMath then atts else
      val result: AttributesImpl = AttributesImpl(atts)
      val isInline: Option[Boolean] = MathML.displayAttribute.optional.get(Sax)(atts)
      MathML.displayAttribute.optionalSetDefault.withValue(checkInline(isInline)).set(Sax)(result)
      result

    super.startElement(uri, localName, qName, attributes)
    pushElement(localName)

  override def endElement(uri: String, localName: String, qName: String): Unit =
    flush()

    super.endElement(uri, localName, qName)
    popElement()

  override def processingInstruction(target: String, data: String): Unit =
    flush()

    super.processingInstruction(target, data)

  override def skippedEntity(name: String): Unit =
    flush()

    super.skippedEntity(name)

  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit =
    if delimiters.isDefined then addToMath(ch.slice(start, start + length).mkString(""))
    else super.ignorableWhitespace(ch, start, length)

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    characters(ch.slice(start, start + length).mkString(""))

  private def characters(chars: String): Unit = delimiters.fold {
    // do not scan code-containing elements for equations
    val currentElementIsExcluded: Boolean = DocBook.codeElements.contains(currentElement)
    val start: Option[(DelimitersAndInput, Int)] =
      if currentElementIsExcluded then None
      else MathFilter.start(allDelimiters, chars)

    start.fold(sendToParent(unescape(chars)))((delimitersStarting: DelimitersAndInput, index: Int) =>
      if index != 0 then sendToParent(unescape(chars.take(index)))
      delimiters = Some(delimitersStarting)
      characters(chars.substring(index + delimitersStarting.start.length))
    )

  }((delimiters: DelimitersAndInput) =>
    MathFilter.findUnescaped(delimiters.end, chars).fold(addToMath(chars))((index: Int) =>
      if index != 0 then addToMath(chars.take(index))
      flush(closedByDelimiter = true)
      characters(chars.substring(index + delimiters.end.length))
    )
  )

  private def unescape(chars: String): String =
    if !processEscapes then chars else chars.replace("\\$", "$")

  // TODO should I use DocBook's <markup role="mathjax"> element or something instead of the MathML one?

  private def flush(closedByDelimiter: Boolean = false): Unit = if delimiters.isDefined then
    if !closedByDelimiter then warning(s"Math '$math' not closed")

    MathFilter.logger.debug(s"MathFilter.flush(): math=$math")

    val input = delimiters.get.input
    val isInline: Option[Boolean] = checkInline(input.isInline)
    val attributes = new AttributesImpl
    MathFilter.inputAttribute.withValue(Input.withInline(input, isInline)).set(Sax)(attributes)

    def mml(): Unit =
      // Note: unless prefix mappings for MathML and MathJax plugin namespaces are delineated properly,
      // math element and its children end up having *two* default namespaces - MathML and DocBook.
      //
      // Note: On Saxon 10 (but not 9!), XInclude namespace, if present on the `<article>`,
      // is added to the `<math>` element - but not its children.
      prefixMapping(MathML.namespace.default)(
        prefixMapping(MathFilter.namespace)(
          element(MathML.namespace.default, MathML.math, atts = attributes)(
            element(MathML.namespace.default, MathML.mrow)(
              element(MathML.namespace.default, MathML.mi)(
                sendToParent(math)
              )))))

    if currentlyInEquationElement then
      mml()
    else element(DocBook.namespace.default, if isInline.contains(true) then "inlineequation" else "informalequation")(
      mml()
    )

    delimiters = None
    math = ""

  private def checkInline(isInline: Option[Boolean]): Option[Boolean] =
    val shouldBeInline: Option[Boolean] =
      if !currentlyInEquationElement then None
      // mark the MathML wrapper as inline if inside this
      else Some(DocBook.inlineEquationElements.contains(currentElement))

    if shouldBeInline.isDefined && isInline.isDefined && (shouldBeInline.get != isInline.get) then
      val should: String = MathML.displayAttribute.toString(shouldBeInline.get)
      val is: String = MathML.displayAttribute.toString(isInline.get)
      warning(s"Display mode conflict: based on context, math should be '$should', but it is marked as '$is'")

    shouldBeInline.orElse(isInline)

object MathFilter:

  private val logger: Logger = LoggerFactory.getLogger(classOf[MathFilter])

  private def start(allDelimiters: Seq[DelimitersAndInput], chars: String): Option[(DelimitersAndInput, Int)] =
    val starts: Seq[(DelimitersAndInput, Int)] = for
      delimiters: DelimitersAndInput <- allDelimiters
      index: Option[Int] = findUnescaped(delimiters.start, chars)
      if index.isDefined
    yield delimiters -> index.get

    starts.sortBy(_._2).headOption

  private def findUnescaped(what: String, chars: String): Option[Int] =
    val index: Int = chars.indexOf(what)
    if index == -1 then None
    else if index == 0 then Some(index)
    else if chars.charAt(index-1) == '\\' then None
    else Some(index)

  val namespace: Namespace = Namespace(
    uri = "http://opentorah.org/mathjax/ns/ext",
    prefix = "mathjax"
  )

  /**
   * Type of the input: TeX, MathML, AsciiMath.
   */
  final class InputAttribute extends Attribute[Input]("input", namespace = namespace, default = Input.MathML):
    override def toString(value: Input): String = value.name

    override def fromString(value: String): Input =
      Input.values.find(_.name == value).getOrElse(throw IllegalArgumentException(s"Unknown input type: $value"))

  @SerialVersionUID(1L)
  val inputAttribute: Attribute.OrDefault[Input] = (new InputAttribute).orDefault

  def unwrap(mathML: Dom.Element): String = mathML
    .getElementsByTagName(MathML.mrow).item(0).asInstanceOf[Dom.Element]
    .getElementsByTagName(MathML.mi  ).item(0).asInstanceOf[Dom.Element]
    .getTextContent
