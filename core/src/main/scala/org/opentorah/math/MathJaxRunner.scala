package org.opentorah.math

import org.opentorah.fop.Svg
import org.opentorah.xml.Dom
import org.slf4j.{Logger, LoggerFactory}
import org.w3c.dom.{Document, Element}
import org.w3c.dom.svg.SVGDocument

abstract class MathJaxRunner:

  final def typeset(mathMLDocument: Document): SVGDocument =
    val element: Element = mathMLDocument.getDocumentElement

    val input: Input = MathFilter.inputAttribute.get(Dom)(element)
    val mathString: String =
      if input == Input.MathML
      then MathML.prettyPrinter.render(Dom)(element)
      else MathFilter.unwrap(element)

    val fontSize: Float = Sizes.fontSizeAttribute.required.get(Dom)(element)

    val svgRaw: String = typeset(
      mathString = mathString,
      input = input,
      fontSize = fontSize
    )

    // Note: chop off parasitic output from NodeJS: 'undefined', Promise<pending> -
    // I guess this is the overall result of the script?
    val svgStart: Int = svgRaw.indexOf("<svg")
    val svgEnd: Int = svgRaw.indexOf("</svg>")
    if (svgStart == -1) || (svgEnd == -1) || (svgEnd < svgStart)
    then throw new IllegalArgumentException(s"No SVG in the typesetter output: $svgRaw")
    val svg: String = svgRaw.substring(svgStart, svgEnd+"</svg>".length)

    MathJaxRunner.logger.debug(s"${MathJaxRunner.logStart}$mathString${MathJaxRunner.logSep}$svg${MathJaxRunner.logEnd}")

    val result: SVGDocument = Svg.fromString(svg)

    // set font size on the resulting SVG - it is needed for the sizes calculations:
    Sizes.fontSizeAttribute.required.withValue(fontSize).set(Dom)(result.getDocumentElement)

    result

  protected def typeset(
    mathString: String,
    input: Input,
    fontSize: Float
  ): String

object MathJaxRunner:

  private val logger: Logger = LoggerFactory.getLogger(classOf[MathJaxRunner])

  val logStart: String = "typesetting ["
  val logSep: String = "]; typesetting result ["
  val logEnd: String = "]"
