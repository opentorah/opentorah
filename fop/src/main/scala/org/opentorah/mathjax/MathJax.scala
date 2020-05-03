package org.opentorah.mathjax

import java.io.File
import org.gradle.api.Project
import org.opentorah.node.{J2V8, Node, NodeDistribution, NodeFromArtifact}
import org.opentorah.xml.{Namespace, Xerces}
import org.slf4j.{Logger, LoggerFactory}
import org.w3c.dom.Document
import org.w3c.dom.svg.SVGDocument

abstract class MathJax(
  val configuration: Configuration
) {

  final def typeset(mathMLDocument: Document): SVGDocument = {
    val input: Input = Input.Attribute.getWithDefault(mathMLDocument)
    val math: String =
      if (input == Input.MathML) Xerces.toString(mathMLDocument) else MathML.unwrap(mathMLDocument)

    val fontSize: Float = Sizes.FontSizeAttribute.doGet(mathMLDocument)

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
    Sizes.FontSizeAttribute.set(fontSize, result)

    result
  }

  protected def typeset(
    options: Map[String, Any],
    outputName: String,
  ): String
}

object MathJax {

  private val logger: Logger = LoggerFactory.getLogger(classOf[MathJax])

  val packageName: String = "mathjax-node"

  object Namespace extends Namespace(
    uri = "http://podval.org/mathjax/ns/ext",
    prefix = "mathjax"
  )

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
    project: Project,
    nodeRoot: File,
    nodeVersion: String,
    overwriteNode: Boolean,
    nodeModulesParent: File,
    overwriteMathJax: Boolean,
    j2v8Parent: Option[File],
    configuration: Configuration
  ): MathJax = {
    val node: NodeFromArtifact = new NodeFromArtifact(
      into = nodeRoot,
      distribution = new NodeDistribution(nodeVersion),
      nodeModulesParent
    )
    node.install(project, overwriteNode)

    get(
      node,
      overwriteMathJax,
      j2v8 = j2v8Parent.flatMap(j2v8Parent => J2V8.install(
        project,
        into = j2v8Parent)),
      configuration
    )
  }

  def get(
    nodeModulesParent: File,
    overwriteMathJax: Boolean,
    configuration: Configuration = new Configuration
  ): MathJax = get(
    node = Node.fromOs(nodeModulesParent).get,
    overwriteMathJax,
    j2v8 = None,
    configuration
  )

  private def get(
    node: Node,
    overwriteMathJax: Boolean,
    j2v8: Option[J2V8],
    configuration: Configuration
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
