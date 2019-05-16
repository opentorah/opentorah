package org.podval.docbook.gradle.mathjax

import org.xml.sax.Attributes
import org.xml.sax.helpers.{AttributesImpl, XMLFilterImpl}
import org.podval.docbook.gradle.DocBookPlugin.DocBookNamespace

final class MathReader extends XMLFilterImpl {
  import MathReader.Mode

  private var elementsStack: List[String] = List.empty

  private def currentElement: String = elementsStack.last

  private var mode: Option[Mode] = None

  private var equation: String = ""

  private def addToEquation(what: String): Unit = equation = equation + what

  private def sendToParent(what: String): Unit = super.characters(what.toCharArray, 0, what.length)

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    characters(ch.slice(start, start + length).mkString(""))

  private def characters(chars: String): Unit = mode.fold {
    if (MathReader.notScannedElements.contains(currentElement)) sendToParent(chars) else {
      val starts: Seq[(Mode, Int)] = for {
        mode <- MathReader.modes
        index = findUnquoted(mode.start, mode.processEscapes, chars)
        if index.isDefined
      } yield mode -> index.get

      starts.sortBy(_._2).headOption.fold {
        sendToParent(chars)
      } { case (modeStarting: Mode, index: Int) =>
        if (index != 0) sendToParent(chars.take(index))
        mode = Some(modeStarting)
        characters(chars.substring(index + modeStarting.start.length))
      }
    }

  } { currentMode: Mode =>
    findUnquoted(currentMode.end, currentMode.processEscapes, chars).fold { addToEquation(chars) } { index: Int =>
      if (index != 0) addToEquation(chars.take(index))
      flush()
      characters(chars.substring(index + currentMode.end.length))
    }
  }

  private def findUnquoted(what: String, processEscapes: Boolean, chars: String): Option[Int] = {
    val index: Int = chars.indexOf(what)
    if (index == -1) None
    else if (index == 0) Some(index)
    else if (processEscapes && chars.charAt(index-1) == '\\') None
    else Some(index)
  }

  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
    if (mode.isDefined) addToEquation(ch.slice(start, start + length).mkString(""))
    else super.ignorableWhitespace(ch, start, length)
  }

  private def flush(): Unit = {
    mode.foreach { currentMode: Mode =>
      if (MathReader.equationElements.contains(currentElement)) mml() else {
        val elementName: String = (if (currentMode.isInline) "inline" else "informal") + "equation"
        element(DocBookNamespace, elementName)( mml() )
      }
    }

    mode = None
    equation = ""
  }

  private def mml(): Unit = {
    val mathAttributes: AttributesImpl = emptyAttributes
    mathAttributes.addAttribute(
      Parameters.MathJaxNameSpace,
      Parameters.Mode.name,
      Parameters.Mode.prefixedName,
      "CDATA",
      mode.get.name
    )

    // NOTE: unless prefix mappings for MathML and MathJax plugin namespaces are delineated properly,
    // math element and its children end up having *two* default namespaces - MathML and DocBook.
    prefixMapping("", MathJaxFopPlugin.MathMLNameSpace) {
      prefixMapping(Parameters.MathJaxAttributePrefix, Parameters.MathJaxNameSpace) {
        element(MathJaxFopPlugin.MathMLNameSpace, MathJaxFopPlugin.MathMLElementName, atts = mathAttributes) {
          element(MathJaxFopPlugin.MathMLNameSpace, "mrow") {
            element(MathJaxFopPlugin.MathMLNameSpace, "mi") {
              sendToParent(equation)
            }}}}}
  }

  // NOTE: in straight XML parsing, empty attribute URI (including the one on the null attribute) is "";
  // in the elements generated here it is null - but this doesn't seem to break anything :)
  private def emptyAttributes: AttributesImpl = new AttributesImpl

  private def element(
    uri: String,
    localName: String,
    atts: Attributes = emptyAttributes
  )(content: => Unit): Unit = {
    val qNameEffective = localName
    super.startElement(uri, localName, qNameEffective, atts)
    content
    super.endElement(uri, localName, qNameEffective)
  }

  private def prefixMapping(
    prefix: String,
    uri: String
  )(content: => Unit): Unit = {
    super.startPrefixMapping(prefix, uri)
    content
    super.endPrefixMapping(prefix)
  }

  // Flush the state when something that interrupts the character sequence is seen:

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
    elementsStack = elementsStack :+ localName
    super.startElement(uri, localName, qName, atts)
  }

  override def endElement(uri: String, localName: String, qName: String): Unit = {
    flush()
    elementsStack = elementsStack.init
    super.endElement(uri, localName, qName)
  }

  override def processingInstruction(target: String, data: String): Unit = {
    flush()
    super.processingInstruction(target, data)
  }

  override def skippedEntity(name: String): Unit = {
    flush()
    super.skippedEntity(name)
  }
}

object MathReader {

  // do not generate DocBook equation wrapper if we are in one already :)
  val equationElements: Set[String] = Set("equation", "informalequation", "inlineequation")

  // do not scan code-containing elements for equations
  val notScannedElements: Set[String] = Set()

  sealed trait Mode {
    def start: String
    def end: String
    def processEscapes: Boolean = false
    def isInline: Boolean
    def name: String = MathJax.Tex.input
  }

  case object TeX extends Mode {
    def start: String = "$$"
    def end: String = "$$"
    def isInline: Boolean = false
  }

  case object LaTeX extends Mode {
    def start: String = "\\("
    def end: String = "\\)"
    def isInline: Boolean = false
  }

  case object TeXInline extends Mode {
    def start: String = "$"
    def end: String = "$"
    def isInline: Boolean = true
  }

  case object LaTeXInline extends Mode {
    def start: String = "\\["
    def end: String = "\\]"
    def isInline: Boolean = true
  }

  case object AsciiMath extends Mode {
    def start: String = "`"
    def end: String = "`"
    def isInline: Boolean = true
    override def name: String = MathJax.AsciiMath.input
  }

  val modes: Seq[Mode] = Seq(TeX, TeXInline, LaTeX, LaTeXInline, AsciiMath)
}
