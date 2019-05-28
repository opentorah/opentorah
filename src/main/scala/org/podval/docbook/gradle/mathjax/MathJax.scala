package org.podval.docbook.gradle.mathjax

import java.io.File

import org.podval.docbook.gradle.xml.Xml
import org.w3c.dom.Document
import org.w3c.dom.svg.SVGDocument

final class MathJax(nodeModulesRoot: File, configuration: MathJax.Configuration) {
  private val configurationMap: Map[String, Any] = MathJax.configurationMap(configuration)

  // TODO if library doesn't load - fall back.
  private val useJ2V8: Boolean = true

  def typeset(mathMLDocument: Document): SVGDocument = {
    val input: Input = Input.Attribute.getWithDefault(mathMLDocument)

    typeset(
      what = if (input == Input.MathML) Xml.toString(mathMLDocument) else MathReader.unwrap(mathMLDocument),
      input = input,
      fontSize = Sizes.FontSizeAttribute.doGet(mathMLDocument)
    )
  }

  def typeset(what: String, input: Input, fontSize: Float): SVGDocument = {
    val svg: String = typeset(what, input, Output.Svg, fontSize.toInt)
    val result: SVGDocument = Svg.fromString(svg)

    // set font size on the resulting SVG - it is needed for the sizes calculations:
    Sizes.FontSizeAttribute.set(fontSize, result)

    result
  }

  // Main typesetting method
  def typeset(math: String, input: Input, output: Output, fontSize: Float): String = {
    val isNode: Boolean = false
    val outputName = output.name(isNode)
    val options: Map[String, Any] = MathJax.optionsMap(
      math = math,
      inputName = input.name,
      outputName = outputName,
      ex = fontSize.toInt // TODO ex, not em? do I need to scale fontSize down?
    )

    if (useJ2V8) {
      // NOTE: some tests failed unless I typeset specific TeX math first; some - even then;
      // re-configuring and forcibly re-starting MathJax before each typeset call breaks the tests even more;
      // sometimes, stopping Gradle daemon helped; once, JVM crashed; once, I got:
      //   Invalid V8 thread access: current thread is Thread[Execution worker for ':',5,main]
      //   while the locker has thread Thread[Execution worker for ':',5,]
      // Conclusion: MathJax has to be created, used and closed by the same thread - duh!
      // For now, I just create, use and dispose a fresh MathJax instance for every typesetting -
      // but should probably do the worker thing mentioned in the J2V8 documentation.
      //
      // Some tests fail if their order is reversed when mathJax instance is re-used;
      // this doesn't look like a threading issue - or maybe whatever I "solved" by using fresh MathJax instance
      // for each typesetting wasn't (just) a threading issue either?
      // NOTE: some tests failed unless I typeset specific TeX math first; some - even then;
      val mathJax: MathJaxJ2V8 = new MathJaxJ2V8(nodeModulesRoot)
      mathJax.configure(configurationMap)
      val result: String = mathJax.typeset(options, outputName)
      mathJax.close()
      result

    } else {
      MathJaxExternal.typeset(configurationMap, options, outputName)
    }
  }
}

object MathJax {

  object Namespace extends org.podval.docbook.gradle.xml.Namespace(
    uri = "http://podval.org/mathjax/ns/ext",
    prefix = "mathjax"
  )

  final class Delimiters(val start: String, val end: String)

  final case class Configuration(
    displayMessages: Boolean = false,        // determines whether Message.Set() calls are logged
    displayErrors: Boolean = true,           // determines whether error messages are shown on the console
    undefinedCharError: Boolean = false,     // determines whether "unknown characters" (i.e., no glyph in the configured fonts) are saved in the error array
    extensions: String = "",                 // a convenience option to add MathJax extensions; example: 'Safe,TeX/noUndefined'
    fontURL: String = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/fonts/HTML-CSS", // for webfont urls in the CSS for HTML output
    font: String = "TeX",                    // possible values are TeX, STIX, STIX-Web, Asana-Math, Neo-Euler, Gyre-Pagella,
                                             // Gyre-Termes and Latin-Modern. Note that not all mathematical characters
                                             // are available in all fonts (e.g., Neo-Euler does not include italic
                                             // characters), so some mathematics may work better in some fonts than in
                                             // others. The STIX-Web font is the most complete.

    // paths: Map[String, String] = Map.empty,  // configures custom path variables (e.g., for third party extensions, cf. test/config-third-party-extensions.js)

    texDelimiters: Seq[Delimiters] = Seq(new Delimiters("$$", "$$"), new Delimiters("\\[", "\\]")),
    texInlineDelimiters: Seq[Delimiters] = Seq(new Delimiters("$", "$"), new Delimiters("\\(", "\\)")),
    asciiMathDelimiters: Seq[Delimiters] = Seq(new Delimiters("`", "`"))
  )

  private def configurationMap(configuration: Configuration): Map[String, Any] = Map(
    "displayMessages"     -> configuration.displayMessages,
    "displayErrors"       -> configuration.displayErrors,
    "undefinedCharError"  -> configuration.undefinedCharError,
    "extensions"          -> configuration.extensions,
    "fontURL"             -> configuration.fontURL,
    // standard MathJax configuration options; see https://docs.mathjax.org for more detail
    "MathJax" -> Map(
      "jax" -> List("input/TeX", "input/MathML", "input/AsciiMath", "output/SVG"),
      // TODO see https://github.com/mathjax/MathJax/blob/master/unpacked/jax/output/SVG/config.js
      // matchFontHeight: true
      // mtextFontInherit: false
      // scale: 100
      // minScaleAdjust: 50
      "SVG" -> Map("font" -> configuration.font)
    )
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

    // an object to store information from multiple calls
    // (e.g., <defs> if useGlobalCache, counter for equation numbering if equationNumbers)
    // state: {}
  )
}
