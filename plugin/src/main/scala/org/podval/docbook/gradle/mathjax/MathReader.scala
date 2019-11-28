package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.plugin.DocBook
import org.podval.docbook.gradle.util.Logger
import org.podval.docbook.gradle.xml.{Namespace, WarningFilter}
import MathML.DisplayAttribute
import Configuration.DelimitersAndInput
import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl
import org.w3c.dom.{Document, Element}

final class MathReader(
  configuration: Configuration,
  logger: Logger
) extends WarningFilter {
  private var elementsStack: List[String] = List.empty
  private def pushElement(localName: String): Unit = elementsStack = elementsStack :+ localName
  private def popElement(): Unit = elementsStack = elementsStack.init

  private def currentElement: String = elementsStack.last
  private def currentlyInEquationElement: Boolean = MathReader.equationElements.contains(currentElement)

  private val allDelimiters: Seq[DelimitersAndInput] = configuration.allDelimiters
  private var delimiters: Option[DelimitersAndInput] = None

  private var math: String = ""
  private def addToMath(what: String): Unit = math = math + what

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

  override def endDocument(): Unit = {
    flush()

    super.endDocument()
  }

  override def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit = {
    flush()

    val isMathMLMath: Boolean = MathML.Namespace.is(uri) && (MathML.math == localName)
    val attributes: Attributes = if (!isMathMLMath) atts else {
      val result: AttributesImpl = new AttributesImpl(atts)
      val isInline: Option[Boolean] = DisplayAttribute.get(MathML.Namespace, atts)
      DisplayAttribute.setWithDefault(MathML.Namespace, checkInline(isInline), result)
      result
    }

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
    if (delimiters.isDefined) addToMath(ch.slice(start, start + length).mkString(""))
    else super.ignorableWhitespace(ch, start, length)
  }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    characters(ch.slice(start, start + length).mkString(""))

  private def characters(chars: String): Unit = delimiters.fold {
    val currentElementIsExcluded: Boolean = MathReader.notScannedElements.contains(currentElement)
    val start: Option[(DelimitersAndInput, Int)] =
      if (currentElementIsExcluded) None
      else MathReader.start(allDelimiters, chars)

    start.fold(sendToParent(unescape(chars))) { case (delimitersStarting: DelimitersAndInput, index: Int) =>
      if (index != 0) sendToParent(unescape(chars.take(index)))
      delimiters = Some(delimitersStarting)
      characters(chars.substring(index + delimitersStarting.start.length))
    }

  } { delimiters: DelimitersAndInput =>
    MathReader.findUnescaped(delimiters.end, chars).fold { addToMath(chars) } { index: Int =>
      if (index != 0) addToMath(chars.take(index))
      flush(closedByDelimiter = true)
      characters(chars.substring(index + delimiters.end.length))
    }
  }

  private def unescape(chars: String): String =
    if (!configuration.processEscapes) chars else chars.replace("\\$", "$")

  private def flush(closedByDelimiter: Boolean = false): Unit = if (delimiters.isDefined) {
    if (!closedByDelimiter) warning(s"Math '$math' not closed")

    logger.debug(s"MathReader.flush(): math=$math")

    val input = delimiters.get.input
    val isInline: Option[Boolean] = checkInline(input.isInline)
    val attributes = new AttributesImpl
    Input.Attribute.set(MathML.Namespace, input.withInline(isInline), attributes)

    def mml(): Unit = {
      // NOTE: unless prefix mappings for MathML and MathJax plugin namespaces are delineated properly,
      // math element and its children end up having *two* default namespaces - MathML and DocBook.
      prefixMapping(MathML.Namespace.default) {
        prefixMapping(MathJax.Namespace) {
          element(MathML.Namespace.default, MathML.math, atts = attributes) {
            element(MathML.Namespace.default, MathML.mrow) {
              element(MathML.Namespace.default, MathML.mi) {
                sendToParent(math)
              }}}}}
    }

    if (currentlyInEquationElement) {
      mml()
    } else element(DocBook.Namespace, if (isInline.contains(true)) "inlineequation" else "informalequation") {
      mml()
    }

    delimiters = None
    math = ""
  }

  private def checkInline(isInline: Option[Boolean]): Option[Boolean] = {
    val shouldBeInline: Option[Boolean] =
      if (!currentlyInEquationElement) None
      else Some(MathReader.inlineEquationElements.contains(currentElement))

    if (shouldBeInline.isDefined && isInline.isDefined && (shouldBeInline.get != isInline.get)) {
      val should: String = DisplayAttribute.toString(shouldBeInline.get)
      val is: String = DisplayAttribute.toString(isInline.get)
      warning(s"Display mode conflict: based on context, math should be '$should', but it is marked as '$is'")
    }

    shouldBeInline.orElse(isInline)
  }
}

object MathReader {

  // do not generate DocBook math wrapper if we are inside one of those
  private val equationElements: Set[String] = Set("equation", "informalequation", "inlineequation")

  // mark the MathML wrapper as inline if inside this
  private val inlineEquationElements: Set[String] = Set("inlineequation")

  // do not scan code-containing elements for equations
  private val notScannedElements: Set[String] = Set()

  private def start(allDelimiters: Seq[DelimitersAndInput], chars: String): Option[(DelimitersAndInput, Int)] = {
    val starts: Seq[(DelimitersAndInput, Int)] = for {
      delimiters <- allDelimiters
      index = findUnescaped(delimiters.start, chars)
      if index.isDefined
    } yield delimiters -> index.get

    starts.sortBy(_._2).headOption
  }

  private def findUnescaped(what: String, chars: String): Option[Int] = {
    val index: Int = chars.indexOf(what)
    if (index == -1) None
    else if (index == 0) Some(index)
    else if (chars.charAt(index-1) == '\\') None
    else Some(index)
  }

  def unwrap(mathMLDocument: Document): String = mathMLDocument.getDocumentElement
    .getElementsByTagName(MathML.mrow).item(0).asInstanceOf[Element]
    .getElementsByTagName(MathML.mi).item(0).asInstanceOf[Element]
    .getTextContent
}
