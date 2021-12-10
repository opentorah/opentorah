package org.opentorah.math

import org.opentorah.fop.Svg
import org.opentorah.xml.Dom
import org.slf4j.{Logger, LoggerFactory}
import org.w3c.dom.{Document, Element}
import org.w3c.dom.svg.SVGDocument

abstract class MathJaxRunner(math: MathConfiguration):

  final def typeset(mathMLDocument: Document): SVGDocument =
    val element: Element = mathMLDocument.getDocumentElement

    val input: Input = MathFilter.inputAttribute.get(Dom)(element)
    val mathString: String =
      if input == Input.MathML then MathML.prettyPrinter.render(Dom)(element)
      else MathFilter.unwrap(element)

    val fontSize: Float = Sizes.fontSizeAttribute.required.get(Dom)(element)

    val outputName: String = Output.Svg.name(isNode = false)

    val svg: String = typeset(
      options = math.mathJax.optionsMap(
        math = mathString,
        inputName = input.name,
        outputName = outputName,
        fontSize = fontSize
      ),
      outputName = outputName
    )

    MathJaxRunner.logger.debug(s"${MathJaxRunner.logStart}$mathString${MathJaxRunner.logSep}$svg${MathJaxRunner.logEnd}")

    val result: SVGDocument = Svg.fromString(svg)

    // set font size on the resulting SVG - it is needed for the sizes calculations:
    Sizes.fontSizeAttribute.required.withValue(fontSize).set(Dom)(result.getDocumentElement)

    result

  protected def typeset(
    options: Map[String, Matchable],
    outputName: String,
  ): String

object MathJaxRunner:

  private val logger: Logger = LoggerFactory.getLogger(classOf[MathJaxRunner])

  val logStart: String = "typesetting ["
  val logSep: String = "]; typesetting result ["
  val logEnd: String = "]"
