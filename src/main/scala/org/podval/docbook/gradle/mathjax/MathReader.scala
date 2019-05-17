package org.podval.docbook.gradle.mathjax

import org.xml.sax.Attributes
import org.xml.sax.helpers.{AttributesImpl, XMLFilterImpl}
import org.podval.docbook.gradle.Namespace
import Namespace.MathML
import org.w3c.dom.{Document, Element}

// Note: I am not going to bother trying to find from the FO context what is the appropriate display mode:
// it is being set here already for all math except for MathML in an included file, in which case the author
// will have to supply it :)
final class MathReader extends XMLFilterImpl {
  import MathReader.Mode

  private var elementsStack: List[String] = List.empty
  private def pushElement(localName: String): Unit = elementsStack = elementsStack :+ localName
  private def popElement(): Unit = elementsStack = elementsStack.init

  private def currentElement: String = elementsStack.last

  private var mode: Option[Mode] = None

  private var equation: String = ""

  private def addToEquation(what: String): Unit = equation = equation + what

  private def sendToParent(what: String): Unit = super.characters(what.toCharArray, 0, what.length)

  private def element(
    namespace: Namespace,
    localName: String,
    atts: Attributes = emptyAttributes
  )(content: => Unit): Unit = {
    val uri = namespace.uri
    val qName = namespace.qName(localName)

    super.startElement(uri, localName, qName, atts)
    pushElement(localName)

    content

    super.endElement(uri, localName, qName)
    popElement()
  }

  private def prefixMapping(namespace: Namespace)(content: => Unit): Unit = {
    val prefix = namespace.prefix

    super.startPrefixMapping(prefix, namespace.uri)
    content
    super.endPrefixMapping(prefix)
  }

  override def startDocument(): Unit = {
    flush()

    super.startDocument()
  }

  override def endDocument(): Unit = {
    flush()

    super.endDocument()
  }

  override def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit = {
    flush()

    val isMathMLMath: Boolean = MathML.is(uri) && (MathML.math == localName)
    val attributes: Attributes = if (!isMathMLMath) atts else setAttributes(
      isInline = Parameter.Display.get(atts),
      attributes = new AttributesImpl(atts)
    )

    super.startElement(uri, localName, qName, attributes)
    pushElement(localName)
  }

  override def endElement(uri: String, localName: String, qName: String): Unit = {
    flush()

    super.endElement(uri, localName, qName)
    popElement()
  }

  override def processingInstruction(target: String, data: String): Unit = {
    flush()

    super.processingInstruction(target, data)
  }

  override def skippedEntity(name: String): Unit = {
    flush()

    super.skippedEntity(name)
  }

  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
    if (mode.isDefined) addToEquation(ch.slice(start, start + length).mkString(""))
    else super.ignorableWhitespace(ch, start, length)
  }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    characters(ch.slice(start, start + length).mkString(""))

  private def characters(chars: String): Unit = mode.fold {
    val currentElementIsExcluded: Boolean = MathReader.notScannedElements.contains(currentElement)
    val start: Option[(Mode, Int)] = if (currentElementIsExcluded) None else {
      val starts: Seq[(Mode, Int)] = for {
        mode <- MathReader.modes
        index = findUnquoted(mode.start, mode.processEscapes, chars)
        if index.isDefined
      } yield mode -> index.get

      starts.sortBy(_._2).headOption
    }

    start.fold(sendToParent(chars)) { case (modeStarting: Mode, index: Int) =>
      if (index != 0) sendToParent(chars.take(index))
      mode = Some(modeStarting)
      characters(chars.substring(index + modeStarting.start.length))
    }

  } { mode: Mode =>
    findUnquoted(mode.end, mode.processEscapes, chars).fold { addToEquation(chars) } { index: Int =>
      if (index != 0) addToEquation(chars.take(index))
      flush()
      characters(chars.substring(index + mode.end.length))
    }
  }

  private def findUnquoted(what: String, processEscapes: Boolean, chars: String): Option[Int] = {
    val index: Int = chars.indexOf(what)
    if (index == -1) None
    else if (index == 0) Some(index)
    else if (processEscapes && chars.charAt(index-1) == '\\') None
    else Some(index)
  }

  private def flush(): Unit = {
    if (mode.isDefined) {
      val isInline: Option[Boolean] = mode.get.isInline
      val attributes: Attributes = setAttributes(isInline, emptyAttributes)

      def mml(): Unit = {
        // NOTE: unless prefix mappings for MathML and MathJax plugin namespaces are delineated properly,
        // math element and its children end up having *two* default namespaces - MathML and DocBook.
        prefixMapping(MathML.default) {
          prefixMapping(Namespace.MathJax) {
            element(MathML.default, MathML.math, atts = attributes) {
              element(MathML.default, MathML.mrow) {
                element(MathML.default, MathML.mi) {
                  sendToParent(equation)
                }}}}}
      }

      val currentlyInEquationElement: Boolean = MathReader.equationElements.contains(currentElement)
      if (currentlyInEquationElement) {
        mml()
      } else element(Namespace.DocBook, if (isInline.contains(true)) "inlineequation" else "informalequation") {
        mml()
      }
    }

    mode = None
    equation = ""
  }

  // NOTE: in straight XML parsing, empty attribute URI (including the one on the null attribute) is "";
  // in the elements generated here it is null - but this doesn't seem to break anything :)
  private def emptyAttributes: AttributesImpl = new AttributesImpl

  private def setAttributes(
    isInline: Option[Boolean],
    attributes: AttributesImpl
  ): Attributes = {
    val shouldBeInline: Option[Boolean] =
      if (!MathReader.equationElements.contains(currentElement)) None
      else Some(MathReader.inlineEquationElements.contains(currentElement))

    // TODO log instead
    val isConflict: Boolean = shouldBeInline.fold(false)(shouldBeInline => isInline.contains(!shouldBeInline))
    if (isConflict) throw new IllegalArgumentException("Wrong display mode!")

    mode.foreach(mode => Parameter.Mode.set(MathML, mode.name, attributes))
    shouldBeInline.orElse(isInline).foreach(isInline => Parameter.Display.set(MathML, isInline, attributes))
    attributes
  }
}

object MathReader {

  // do not generate DocBook equation wrapper if we are inside one of those
  val equationElements: Set[String] = Set("equation", "informalequation", "inlineequation")

  // mark the MathML wrapper as inline if inside this
  val inlineEquationElements: Set[String] = Set("inlineequation")

  // do not scan code-containing elements for equations
  val notScannedElements: Set[String] = Set()

  sealed trait Mode {
    def start: String
    def end: String
    def processEscapes: Boolean = false
    def isInline: Option[Boolean]
    def name: String = MathJax.Tex.input
  }

  case object TeX extends Mode {
    def start: String = "$$"
    def end: String = "$$"
    def isInline: Option[Boolean] = Some(false)
  }

  case object LaTeX extends Mode {
    def start: String = "\\["
    def end: String = "\\]"
    def isInline: Option[Boolean] = Some(false)
  }

  // TODO do not enable by default - and if enabled, process escapes: \$ -> *outside* of TeX math!
  case object TeXInline extends Mode {
    def start: String = "$"
    def end: String = "$"
    def isInline: Option[Boolean] = Some(true)
  }

  case object LaTeXInline extends Mode {
    def start: String = "\\("
    def end: String = "\\)"
    def isInline: Option[Boolean] = Some(true)
  }

  case object AsciiMath extends Mode {
    def start: String = "`"
    def end: String = "`"
    def isInline: Option[Boolean] = None
    override def name: String = MathJax.AsciiMath.input
  }

  val modes: Seq[Mode] = Seq(TeX, TeXInline, LaTeX, LaTeXInline, AsciiMath)

  def unwrap(mathMLDocument: Document): String = mathMLDocument.getDocumentElement
    .getElementsByTagName(MathML.mrow).item(0).asInstanceOf[Element]
    .getElementsByTagName(MathML.mi).item(0).asInstanceOf[Element]
    .getTextContent
}
