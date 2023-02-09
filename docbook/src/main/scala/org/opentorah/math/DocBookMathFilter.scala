package org.opentorah.math

import org.opentorah.docbook.DocBook
import org.opentorah.xml.{Attribute, Dom, Namespace, Sax}
import org.slf4j.{Logger, LoggerFactory}
import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl

final class DocBookMathFilter(
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
    attrs: Attributes = new AttributesImpl
  )(content: => Unit): Unit =
    val uri = namespace.uri
    val qName = namespace.qName(localName)

    super.startElement(uri, localName, qName, attrs)
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

  override def startElement(uri: String, localName: String, qName: String, attrs: Attributes): Unit =
    flush()

    // TODO remove?
    val isMathMLMath: Boolean = (uri == MathML.namespace.uri) && (localName == MathML.math)
    val attributes: Attributes = if !isMathMLMath then attrs else
      val result: AttributesImpl = AttributesImpl(attrs)
      val display: Option[Input.Display] = DocBookMathFilter.displayAttribute.optional.get(Sax)(attrs)
      DocBookMathFilter.displayAttribute.required.withValueOption(checkDisplay(display)).set(Sax)(result)
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
      else DocBookMathFilter.start(allDelimiters, chars)

    start.fold(sendToParent(unescape(chars)))((delimitersStarting: DelimitersAndInput, index: Int) =>
      if index != 0 then sendToParent(unescape(chars.take(index)))
      delimiters = Some(delimitersStarting)
      characters(chars.substring(index + delimitersStarting.start.length))
    )

  }((delimiters: DelimitersAndInput) =>
    DocBookMathFilter.findUnescaped(delimiters.end, chars).fold(addToMath(chars))((index: Int) =>
      if index != 0 then addToMath(chars.take(index))
      flush(closedByDelimiter = true)
      characters(chars.substring(index + delimiters.end.length))
    )
  )

  private def unescape(chars: String): String =
    if !processEscapes then chars else chars.replace("\\$", "$")

  // TODO should I use DocBook's <markup role="mathjax"> element or something instead of the MathML one?

  private def flush(closedByDelimiter: Boolean = false): Unit = if delimiters.isDefined then
    if !closedByDelimiter then warning(s"Math '$math' not closed!")

    DocBookMathFilter.logger.debug(s"MathFilter.flush(): math=$math")

    val input: Input = delimiters.get.input
    val display: Option[Input.Display] = checkDisplay(input.display)

    val attributes: AttributesImpl = new AttributesImpl
    DocBookMathFilter.inputTypeAttribute.orDefault.withValue(input.inputType).set(Sax)(attributes)
    DocBookMathFilter.displayAttribute.optional.withValue(display).set(Sax)(attributes)

    def mml(): Unit =
      // TODO
      // Note: unless prefix mappings for MathML and MathJax plugin namespaces are delineated properly,
      // math element and its children end up having *two* default namespaces - MathML and DocBook.
      //
      // Note: On Saxon 10 (but not 9!), XInclude namespace, if present on the `<article>`,
      // is added to the `<math>` element - but not its children.
      prefixMapping(MathML.namespace.default)(
        prefixMapping(DocBookMathFilter.namespace)(
          element(MathML.namespace.default, MathML.math, attrs = attributes)(
            element(MathML.namespace.default, MathML.mrow)(
              element(MathML.namespace.default, MathML.mi)(
                sendToParent(math)
              )))))

    if currentlyInEquationElement then
      mml()
    else element(DocBook.namespace.default, if Input.Display.isBlock(display) then "informalequation" else "inlineequation")(
      mml()
    )

    delimiters = None
    math = ""

  private def checkDisplay(is: Option[Input.Display]): Option[Input.Display] =
    val shouldBe: Option[Input.Display] =
      if !currentlyInEquationElement then None
      // mark the MathML wrapper as inline if inside this
      else Some(if DocBook.inlineEquationElements.contains(currentElement) then Input.Display.Inline else Input.Display.Block)

    if shouldBe.isDefined && is.isDefined && (shouldBe.get != is.get) then
      warning(s"Display mode conflict: based on context, math should be '${shouldBe.get.name}', but it is marked as '${is.get.name}'")

    shouldBe.orElse(is)

object DocBookMathFilter:

  private val logger: Logger = LoggerFactory.getLogger(classOf[DocBookMathFilter])

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
  
  def unwrap(mathML: Dom.Element): String = mathML
    .getElementsByTagName(MathML.mrow).item(0).asInstanceOf[Dom.Element]
    .getElementsByTagName(MathML.mi  ).item(0).asInstanceOf[Dom.Element]
    .getTextContent

  def apply(math: MathConfiguration): DocBookMathFilter =
    def withInput(values: Seq[Delimiters], input: Input): Seq[DelimitersAndInput] =
      for delimiters <- values yield DelimitersAndInput(delimiters, input)

    val allDelimiters: Seq[DelimitersAndInput] =
      withInput(math.texDelimiters      , Input(Input.Type.Tex, Some(Input.Display.Block ))) ++
      withInput(math.texInlineDelimiters, Input(Input.Type.Tex, Some(Input.Display.Inline))) ++
      withInput(math.asciiMathDelimiters, Input(Input.Type.AsciiMath, None))

    new DocBookMathFilter(
      allDelimiters = allDelimiters.sortWith((l: DelimitersAndInput, r: DelimitersAndInput) => l.start.length > r.start.length),
      processEscapes = math.processEscapes.contains(true)
    )

  val namespace: Namespace = Namespace(
    uri = "http://opentorah.org/mathjax/ns/ext",
    prefix = "mathjax"
  )

  @SerialVersionUID(1L)
  val inputTypeAttribute: Attribute[Input.Type] = Attribute.EnumeratedAttribute[Input.Type](
    name = "inputType",
    namespace = DocBookMathFilter.namespace,
    values = Input.Type.values.toIndexedSeq,
    default = Input.Type.MathML,
    getName = _.name
  )

  // Note: this attribute is always used within its native MathML.namespace, so I set its namespace to No.
  @SerialVersionUID(1L)
  val displayAttribute: Attribute[Input.Display] = Attribute.EnumeratedAttribute[Input.Display](
    name = "display",
    values = Input.Display.values.toIndexedSeq,
    default = Input.Display.default,
    getName = _.name
  )
