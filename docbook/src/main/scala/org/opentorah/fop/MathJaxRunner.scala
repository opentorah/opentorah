package org.opentorah.fop

import org.opentorah.mathjax.{Input, MathFilter, MathJax, MathJaxConfiguration, MathML, Output}
import org.opentorah.xml.Dom
import org.slf4j.{Logger, LoggerFactory}
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.{Document, Element}
import java.io.File

abstract class MathJaxRunner(
  mathJax: MathJax,
  val configuration: MathJaxConfiguration
):

  final def typeset(mathMLDocument: Document): SVGDocument =
    val element: Element = mathMLDocument.getDocumentElement

    val input: Input = MathFilter.inputAttribute.get(Dom)(element)
    val math: String =
      if input == Input.MathML then MathML.prettyPrinter.render(Dom)(element)
      else MathFilter.unwrap(element)

    val fontSize: Float = Sizes.fontSizeAttribute.required.get(Dom)(element)

    val outputName: String = Output.Svg.name(isNode = false)

    val svg: String = typeset(
      options = mathJax.optionsMap(
        math = math,
        inputName = input.name,
        outputName = outputName,
        fontSize = fontSize
      ),
      outputName = outputName
    )

    MathJaxRunner.logger.debug(s"${MathJaxRunner.logStart}$math${MathJaxRunner.logSep}$svg${MathJaxRunner.logEnd}")

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

  def get(
    nodeModulesParent: File,
    overwriteMathJax: Boolean,
    mathJax: MathJax,
    configuration: MathJaxConfiguration = new MathJaxConfiguration
  ): MathJaxRunner = get(
    node = Node.fromOs(nodeModulesParent).get,
    overwriteMathJax,
    j2v8 = None,
    mathJax,
    configuration
  )

  def get(
    node: Node,
    overwriteMathJax: Boolean,
    j2v8: Option[J2V8],
    mathJax: MathJax,
    configuration: MathJaxConfiguration
  ): MathJaxRunner =
    // Make sure MathJax is installed
    node.npmInstall(mathJax.packageName, overwriteMathJax)

    // If J2V8 is configured to be used, is available and actually loads - we use it;
    // otherwise each typesetting is done by calling Node in a separate process.
    val useJ2V8: Boolean = j2v8.isDefined && j2v8.get.load()
    if useJ2V8 then J2V8MathJaxRunner(node, mathJax, configuration)
    else ExternalMathJaxRunner(node,mathJax,  configuration)
