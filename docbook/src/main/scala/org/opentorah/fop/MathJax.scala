package org.opentorah.fop

import org.opentorah.docbook.MathFilter
import org.opentorah.mathjax.{Input, MathJaxConfiguration, MathML, Output}
import org.opentorah.xml.{Attribute, Namespace}
import org.slf4j.{Logger, LoggerFactory}
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.{Document, Element}
import java.io.File

abstract class MathJax(
  val configuration: MathJaxConfiguration
) {

  final def typeset(mathMLDocument: Document): SVGDocument = {
    val element: Element = mathMLDocument.getDocumentElement

    val input: Input = MathJax.inputAttribute.get(element)
    val math: String =
      if (input == Input.MathML) MathML.prettyPrinter.render(element)
      else MathFilter.unwrap(element)

    val fontSize: Float = Sizes.fontSizeAttribute.required.get(element)

    val outputName: String = Output.Svg.name(isNode = false)

    val ex: Int = (fontSize * Sizes.mathJaxExInEms).toInt

    val options: Map[String, Any] = MathJax.optionsMap(
      math = math,
      inputName = input.name,
      outputName = outputName,
      ex = ex
    )

    val svg: String = typeset(options, outputName)

    MathJax.logger.debug(s"${MathJax.logStart}$math${MathJax.logSep}$svg${MathJax.logEnd}")

    val result: SVGDocument = Svg.fromString(svg)

    // set font size on the resulting SVG - it is needed for the sizes calculations:
    Sizes.fontSizeAttribute.required.withValue(fontSize).set(result.getDocumentElement)

    result
  }

  protected def typeset(
    options: Map[String, Any],
    outputName: String,
  ): String
}

object MathJax {

  val namespace: Namespace = Namespace(
    uri = "http://opentorah.org/mathjax/ns/ext",
    prefix = "mathjax"
  )

  /**
   * Type of the input: TeX, MathML, AsciiMath.
   */
  final class InputAttribute extends Attribute[Input]("input", namespace = namespace, default = Input.MathML) {
    override def toString(value: Input): String = value.name

    override def fromString(value: String): Input =
      Input.values.find(_.name == value).getOrElse(throw new IllegalArgumentException(s"Unknown input type: $value"))
  }

  @SerialVersionUID(1L)
  val inputAttribute: Attribute.OrDefault[Input] = (new InputAttribute).orDefault

  private val logger: Logger = LoggerFactory.getLogger(classOf[MathJax])

  val packageName: String = "mathjax-node"

  private def optionsMap(math: String, inputName: String, outputName: String, ex: Int): Map[String, Any] = Map(
    "useFontCache"    -> true,       // use <defs> and <use> in svg output ('true' by default)?
    "useGlobalCache"  -> false,      // use common <defs> for all equations?
    "linebreaks"      -> false,      // automatic linebreaking
    "speakText"       -> false,      // add textual alternative (for TeX/asciimath the input string, for MathML a dummy string)?
    "xmlns"           -> "mml",      // the namespace to use for MathML
    "timeout"         -> 10 * 1000,  // 10 second timeout before restarting MathJax
    "width"           -> 100,        // width of container (in ex) for linebreaking and tags
    "cjkCharWidth"    -> 13,         // width of CJK character
    "equationNumbers" -> "none",     // automatic equation numbering ("none", "AMS" or "all")
    "ex"              -> ex,         // ex-size in pixels
    "format"          -> inputName,  // the input format (TeX, inline-TeX, AsciiMath, or MathML)
    "math"            -> math,       // the math string to typeset
    outputName        -> true,       // which output format to produce
    "css"             -> false       // generate CSS for HTML output?
  )

  val logStart: String = "typesetting ["
  val logSep: String = "]; typesetting result ["
  val logEnd: String = "]"

  def get(
    nodeModulesParent: File,
    overwriteMathJax: Boolean,
    configuration: MathJaxConfiguration = new MathJaxConfiguration
  ): MathJax = get(
    node = Node.fromOs(nodeModulesParent).get,
    overwriteMathJax,
    j2v8 = None,
    configuration
  )

  def get(
    node: Node,
    overwriteMathJax: Boolean,
    j2v8: Option[J2V8],
    configuration: MathJaxConfiguration
  ): MathJax = {
    // Make sure MathJax is installed
    node.npmInstall(MathJax.packageName, overwriteMathJax)

    // If J2V8 is configured to be used, is available and actually loads - we use it;
    // otherwise each typesetting is done by calling Node in a separate process.
    val useJ2V8: Boolean = j2v8.isDefined && j2v8.get.load()
    if (useJ2V8) new J2V8MathJax(node, configuration)
    else new ExternalMathJax(node, configuration)
  }
}
