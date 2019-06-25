package org.podval.docbook.gradle.mathjax

import java.io.File

import org.gradle.api.Project
import org.podval.docbook.gradle.util.{Architecture, Logger, Os}
import org.podval.docbook.gradle.xml.Xml
import org.w3c.dom.Document
import org.w3c.dom.svg.SVGDocument

abstract class MathJax(
  node: Node,
  val configuration: Configuration,
  logger: Logger
) {

  final def typeset(mathMLDocument: Document): SVGDocument = {
    val input: Input = Input.Attribute.getWithDefault(mathMLDocument)
    val math: String =
      if (input == Input.MathML) Xml.toString(mathMLDocument) else MathReader.unwrap(mathMLDocument)

    val fontSize: Float = Sizes.FontSizeAttribute.doGet(mathMLDocument)

    val outputName: String = Output.Svg.name(isNode = false)

    val options: Map[String, Any] = MathJax.optionsMap(
      math = math,
      inputName = input.name,
      outputName = outputName,
      ex = fontSize.toInt // TODO ex, not em? do I need to scale fontSize down?
    )

    val svg: String = typeset(options, outputName)

    logger.info(s"${MathJax.logStart}$math${MathJax.logSep}$svg${MathJax.logEnd}")

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

  object Namespace extends org.podval.docbook.gradle.xml.Namespace(
    uri = "http://podval.org/mathjax/ns/ext",
    prefix = "mathjax"
  )

  trait Factory {
    def get(
      node: Node,
      configuration: Configuration,
      logger: Logger
    ): MathJax
  }

  def get(
    project: Project,
    os: Os,
    arch: Architecture,
    nodeRoot: File,
    useJ2V8: Boolean,
    j2v8LibraryDirectory: File,
    configuration: Configuration,
    logger: Logger
  ): MathJax = {
    // make sure Node and MathJax are installed
    val node: Node = Node.install(project, os, arch, nodeRoot, logger)

    // If J2V8 is configured to be used, is available and actually loads - we use it;
    // otherwise each typesetting is done by calling Node in a separate process.
    val mathJaxFactory: MathJax.Factory =
      if (useJ2V8 && J2V8.load(project, os, arch, j2v8LibraryDirectory, logger)) J2V8MathJax
      else ExternalMathJax

    mathJaxFactory.get(node, configuration, logger)
  }

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

    // an object to store information from multiple calls
    // (e.g., <defs> if useGlobalCache, counter for equation numbering if equationNumbers)
    // state: {}
  )

  val logStart: String = "typesetting ["
  val logSep: String = "]; typesetting result ["
  val logEnd: String = "]"
}
