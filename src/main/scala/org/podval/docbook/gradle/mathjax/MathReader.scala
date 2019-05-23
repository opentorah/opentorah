package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.DocBook
import org.podval.docbook.gradle.xml.Namespace
import org.xml.sax.{Attributes, Locator, SAXParseException}
import org.xml.sax.helpers.{AttributesImpl, XMLFilterImpl}
import org.w3c.dom.{Document, Element}

// Note: I am not going to bother trying to find from the FO context what is the appropriate display mode:
// it is being set here already for all math except for MathML in an included file, in which case the author
// will have to supply it :)
// (Besides, I am not even registering the handler, so that won't work anyway.)
final class MathReader(mathJaxConfiguration: MathJaxConfiguration) extends XMLFilterImpl {
  import MathJaxConfiguration.Mode

  private var locator: Option[Locator] = None
  private def warning(message: String): Unit = getErrorHandler.warning(new SAXParseException(message, locator.orNull))

  private var elementsStack: List[String] = List.empty
  private def pushElement(localName: String): Unit = elementsStack = elementsStack :+ localName
  private def popElement(): Unit = elementsStack = elementsStack.init

  private def currentElement: String = elementsStack.last
  private def currentlyInEquationElement: Boolean = MathReader.equationElements.contains(currentElement)

  private var mode: Option[Mode] = None

  private var equation: String = ""

  private def addToEquation(what: String): Unit = equation = equation + what

  private def sendToParent(what: String): Unit = super.characters(what.toCharArray, 0, what.length)

  private def element(
    namespace: Namespace,
    localName: String,
    atts: Attributes = new AttributesImpl
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

  override def setDocumentLocator(locator: Locator): Unit = {
    super.setDocumentLocator(locator)
    this.locator = Some(locator)
  }

  override def endDocument(): Unit = {
    flush()

    super.endDocument()
  }

  override def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit = {
    flush()

    val isMathMLMath: Boolean = MathML.is(uri) && (MathML.math == localName)
    val attributes: Attributes = if (!isMathMLMath) atts else setAttributes(
      isInline = DisplayAttribute.get(atts),
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
    val start: Option[(Mode, Int)] =
      if (currentElementIsExcluded) None
      else mathJaxConfiguration.start(chars)

    start.fold(sendToParent(chars)) { case (modeStarting: Mode, index: Int) =>
      if (index != 0) sendToParent(chars.take(index))
      mode = Some(modeStarting)
      characters(chars.substring(index + modeStarting.start.length))
    }

  } { mode: Mode =>
    mode.findEnd(chars).fold { addToEquation(chars) } { index: Int =>
      if (index != 0) addToEquation(chars.take(index))
      flush(closedByDelimeter = true)
      characters(chars.substring(index + mode.end.length))
    }
  }

  private def flush(closedByDelimeter: Boolean = false): Unit = {
    if (mode.isDefined) {
      if (!closedByDelimeter) warning(s"Equation '$equation' not closed")
      val isInline: Option[Boolean] = mode.get.isInline
      val attributes: Attributes = setAttributes(
        isInline = isInline,
        attributes = new AttributesImpl
      )

      def mml(): Unit = {
        // NOTE: unless prefix mappings for MathML and MathJax plugin namespaces are delineated properly,
        // math element and its children end up having *two* default namespaces - MathML and DocBook.
        prefixMapping(MathML.default) {
          prefixMapping(MathJaxNamespace) {
            element(MathML.default, MathML.math, atts = attributes) {
              element(MathML.default, MathML.mrow) {
                element(MathML.default, MathML.mi) {
                  sendToParent(equation)
                }}}}}
      }

      if (currentlyInEquationElement) {
        mml()
      } else element(DocBook, if (isInline.contains(true)) "inlineequation" else "informalequation") {
        mml()
      }
    }

    mode = None
    equation = ""
  }

  // Note: in straight XML parsing, empty attribute URI (including the one on the null attribute) is "";
  // in the elements generated here it is null - but this doesn't seem to break anything :)
  private def setAttributes(
    isInline: Option[Boolean],
    attributes: AttributesImpl
  ): Attributes = {
    val shouldBeInline: Option[Boolean] =
      if (!currentlyInEquationElement) None
      else Some(MathReader.inlineEquationElements.contains(currentElement))

    if (shouldBeInline.isDefined && isInline.isDefined && (shouldBeInline.get != isInline.get)) {
      val should: String = DisplayAttribute.toString(shouldBeInline.get)
      val is: String = DisplayAttribute.toString(isInline.get)
      warning(s"Display mode conflict: based on context, math should be '$should', but it is marked as '$is'")
    }

    DisplayAttribute.setWithDefault(MathML, shouldBeInline.orElse(isInline), attributes)

    ModeAttribute.setWithDefault(MathML, mode.map(_.input).orElse(ModeAttribute.get(attributes)), attributes)

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

  def unwrap(mathMLDocument: Document): String = mathMLDocument.getDocumentElement
    .getElementsByTagName(MathML.mrow).item(0).asInstanceOf[Element]
    .getElementsByTagName(MathML.mi).item(0).asInstanceOf[Element]
    .getTextContent
}
