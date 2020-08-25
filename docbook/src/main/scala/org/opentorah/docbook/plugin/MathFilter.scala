package org.opentorah.docbook.plugin

import org.opentorah.docbook.DocBook
import org.opentorah.mathjax.Configuration.DelimitersAndInput
import org.opentorah.mathjax.MathML.displayAttribute
import org.opentorah.mathjax.{Configuration, Input, MathJax, MathML}
import org.opentorah.xml.{Namespace, WarningFilter}
import org.slf4j.{Logger, LoggerFactory}
import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl

final class MathFilter(
  configuration: Configuration
) extends WarningFilter {
  private var elementsStack: List[String] = List.empty
  private def pushElement(localName: String): Unit = elementsStack = elementsStack :+ localName
  private def popElement(): Unit = elementsStack = elementsStack.init

  private def currentElement: String = elementsStack.last
  private def currentlyInEquationElement: Boolean = MathFilter.equationElements.contains(currentElement)

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
    val prefix: String = namespace.getPrefix.getOrElse("")

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

    val isMathMLMath: Boolean = (uri == MathML.namespace.uri) && (localName == MathML.math)
    val attributes: Attributes = if (!isMathMLMath) atts else {
      val result: AttributesImpl = new AttributesImpl(atts)
      val isInline: Option[Boolean] = displayAttribute.inNamespace(MathML.namespace).get(atts)
      displayAttribute.inNamespace(MathML.namespace).withValue(checkInline(isInline)).set(result)
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
    val currentElementIsExcluded: Boolean = MathFilter.notScannedElements.contains(currentElement)
    val start: Option[(DelimitersAndInput, Int)] =
      if (currentElementIsExcluded) None
      else MathFilter.start(allDelimiters, chars)

    start.fold(sendToParent(unescape(chars))) { case (delimitersStarting: DelimitersAndInput, index: Int) =>
      if (index != 0) sendToParent(unescape(chars.take(index)))
      delimiters = Some(delimitersStarting)
      characters(chars.substring(index + delimitersStarting.start.length))
    }

  } { delimiters: DelimitersAndInput =>
    MathFilter.findUnescaped(delimiters.end, chars).fold { addToMath(chars) } { index: Int =>
      if (index != 0) addToMath(chars.take(index))
      flush(closedByDelimiter = true)
      characters(chars.substring(index + delimiters.end.length))
    }
  }

  private def unescape(chars: String): String =
    if (!configuration.processEscapes) chars else chars.replace("\\$", "$")

  private def flush(closedByDelimiter: Boolean = false): Unit = if (delimiters.isDefined) {
    if (!closedByDelimiter) warning(s"Math '$math' not closed")

    MathFilter.logger.debug(s"MathFilter.flush(): math=$math")

    val input = delimiters.get.input
    val isInline: Option[Boolean] = checkInline(input.isInline)
    val attributes = new AttributesImpl
    Input.attribute.inNamespace(MathML.namespace).withValue(input.withInline(isInline)).set(attributes)

    def mml(): Unit = {
      // Note: unless prefix mappings for MathML and MathJax plugin namespaces are delineated properly,
      // math element and its children end up having *two* default namespaces - MathML and DocBook.
      //
      // Note: On Saxon 10 (but not 9!), XInclude namespace, if present on the `<article>`,
      // is added to the `<math>` element - but not its children.
      prefixMapping(MathML.namespace.default) {
        prefixMapping(MathJax.namespace) {
          element(MathML.namespace.default, MathML.math, atts = attributes) {
            element(MathML.namespace.default, MathML.mrow) {
              element(MathML.namespace.default, MathML.mi) {
                sendToParent(math)
              }}}}}
    }

    if (currentlyInEquationElement) {
      mml()
    } else element(DocBook.namespace.default, if (isInline.contains(true)) "inlineequation" else "informalequation") {
      mml()
    }

    delimiters = None
    math = ""
  }

  private def checkInline(isInline: Option[Boolean]): Option[Boolean] = {
    val shouldBeInline: Option[Boolean] =
      if (!currentlyInEquationElement) None
      else Some(MathFilter.inlineEquationElements.contains(currentElement))

    if (shouldBeInline.isDefined && isInline.isDefined && (shouldBeInline.get != isInline.get)) {
      val should: String = displayAttribute.toString(shouldBeInline.get)
      val is: String = displayAttribute.toString(isInline.get)
      warning(s"Display mode conflict: based on context, math should be '$should', but it is marked as '$is'")
    }

    shouldBeInline.orElse(isInline)
  }
}

object MathFilter {

  private val logger: Logger = LoggerFactory.getLogger(classOf[MathFilter])

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
}
