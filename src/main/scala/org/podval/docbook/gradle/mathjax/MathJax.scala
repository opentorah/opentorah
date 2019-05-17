package org.podval.docbook.gradle.mathjax

import java.io.File
import java.nio.file.Files

import com.eclipsesource.v8.{NodeJS, V8, V8Array, V8Function, V8Object}
import com.eclipsesource.v8.utils.V8ObjectUtils
import scala.collection.JavaConverters._

final class MathJax(nodeModulesRoot: File) {

  private val nodeJS: NodeJS = {
    MathJax.loadV8()
    NodeJS.createNodeJS()
  }

  private def v8: V8 = nodeJS.getRuntime
  private val mathJaxNode: V8Object = nodeJS.require(new File(nodeModulesRoot, "node_modules/mathjax-node"))

  def configure(configuration: MathJax.Config): Unit = {
    val args: V8Array = V8ObjectUtils.toV8Array(v8, List(Map(
      "displayMessages"     -> configuration.displayMessages,
      "displayErrors"       -> configuration.displayErrors,
      "undefinedCharError"  -> configuration.undefinedCharError,
      "extensions"          -> configuration.extensions,
      "fontURL"             -> configuration.fontURL,
      // standard MathJax configuration options; see https://docs.mathjax.org for more detail
      "MathJax" -> Map(
        "jax" -> List("input/TeX", "input/MathML", "input/AsciiMath", "output/SVG").asJava,
        "SVG" -> Map("font" -> configuration.font).asJava
      ).asJava
    ).asJava).asJava)

    mathJaxNode.executeVoidFunction("config", args)

    args.release()
  }

  // This is done automatically when typeset is first called.
//  private def start(): Unit = {
//    val args = new V8Array(v8)
//    mathJaxNode.executeVoidFunction("start", args)
//    args.release()
//  }

  def typeset2String(math: String, input: MathJax.Input, output: MathJax.Output, ex: Int): String = {
    val data: V8Object = typeset(math, input, output, isNode = false, ex)
    val result: String = data.getString(output.output)
    data.release()
    result
  }

  def typeset2Dom(math: String, input: MathJax.Input, output: MathJax.Output, ex: Int): V8Object = {
    val data: V8Object = typeset(math, input, output, isNode = true, ex)
    val result: V8Object = data.getObject(output.nodeOutput)
    data.release()
    result
  }

  private def typeset(math: String, input: MathJax.Input, output: MathJax.Output, isNode: Boolean, ex: Int): V8Object = {
    var result: V8Object = null

    val options: V8Object = V8ObjectUtils.toV8Object(v8, Map(
      "useFontCache"        -> true,         // use <defs> and <use> in svg output ('true' by default)?
      "useGlobalCache"      -> false,        // use common <defs> for all equations?
      "linebreaks"          -> false,        // automatic linebreaking
      "speakText"           -> false,        // add textual alternative (for TeX/asciimath the input string, for MathML a dummy string)?
      "xmlns"               -> "mml",        // the namespace to use for MathML
      "timeout"             -> 10 * 1000,    // 10 second timeout before restarting MathJax
      "width"               -> 100,          // width of container (in ex) for linebreaking and tags
      "cjkCharWidth"        -> 13,           // width of CJK character
      "equationNumbers"     -> "none",       // automatic equation numbering ("none", "AMS" or "all")
      "ex"                  -> ex,           // ex-size in pixels
      "format"              -> input.input,  // the input format (TeX, inline-TeX, AsciiMath, or MathML)
      "math"                -> math,         // the math string to typeset
      output.output(isNode) -> true,
      "css"                 -> output.css    // generate CSS for HTML output?
      //    state: {},                       // an object to store information from multiple calls (e.g.,
                                             // <defs> if useGlobalCache, counter for equation numbering if equationNumbers ar )
    ).asJava)

    //  The typeset method expects a configuration object options and optionally a callback.
    //  If no callback is passed, it will return a Promise.
    //
    //  Once called, typeset can be called repeatedly and will optionally store information across calls (see state below).
    //
    //  Promise.resolve(result,options) / Promise.reject(errors) / callback(result, options)
    //  mathjax-node returns two objects to Promise.resolve or callback: a result object and the original input options.
    //
    //  The result object will contain (at most) the following structure:
    //    errors:                     // an array of MathJax error messages if any errors occurred
    //    (mml|html|svg)[Node]:       // a string or jsdom of the markup requested
    //    css:                        // a string of CSS if HTML was requested
    //    style:                      // a string of CSS inline style if SVG requested
    //    height:                     // a string containing the height of the SVG output if SVG was requested
    //    width:                      // a string containing the width of the SVG output if SVG was requested
    //    speakText:                  // a string of speech text if requested
    //
    //    state: {                    // the state object (if useGlobalCache or equationNumbers is set)
    //      glyphs:            // a collection of glyph data
    //      defs :             // a string containing SVG def elements
    //      AMS: {
    //        startNumber:  // the current starting equation number
    //        labels:       // the set of labels
    //        IDs:          // IDs used in previous equations
    //      }
    //  }

    //  If the errors array is non-empty, the Promise will reject, and be passed the errors array.
    //  The options contains the configuration object passed to typeset; this can be useful for passing other data along or
    //  for identifying which typeset() call is associated with this (callback) call (in case you use the same callback
    //  function for more than one typeset()).
    val callback: V8Function = new V8Function(v8, (receiver: V8Object, parameters: V8Array) => {
      result = parameters.getObject(0)
      val errors: V8Array = result.getArray("errors")
      if (!errors.isUndefined)
        throw new IllegalArgumentException(V8ObjectUtils.toList(errors).asScala.map(_.asInstanceOf[String]).mkString("\n"))
      errors.release()
      null
    })

    val args: V8Array = V8ObjectUtils.toV8Array(v8, List(options, callback).asJava)
    val callResult: V8Object = mathJaxNode.executeObjectFunction("typeset", args)

    var done: Boolean = false
    while (!done) {
      if (nodeJS.isRunning) nodeJS.handleMessage()
//      else if (result == null) Thread.sleep(10)
      else done = true
    }

    options.release()
    callback.release()
    args.release()
    callResult.release()

    result
  }

  def close() {
    mathJaxNode.release()
    nodeJS.release()
  }
}

object MathJax {

  def loadV8(): Unit = {
    if (!V8.isLoaded) V8.createV8Runtime(
      "dummy",
      Files.createTempDirectory("j2v8").toAbsolutePath.toString
    ).release()
  }

  sealed trait Input {
    def input: String
  }

  sealed trait Output {
    def output: String
    final def nodeOutput: String = output + "Node"
    final def output(isNode: Boolean): String = if (isNode) nodeOutput else output
    def css: Boolean = false
  }

  case object Tex extends Input {
    override val input: String = "TeX"
  }

  case object TexInline extends Input {
    override val input: String = "inline-TeX"
  }

  case object AsciiMath extends Input {
    override val input: String = "AsciiMath"
  }

  case object MathML extends Input with Output {
    override val input: String = "MathML"
    override val output: String = "mml"
  }

  // For SVG, width, height and style attributes are repeated in separate keys of the returned data.
  case object Svg extends Output {
    override val output: String = "svg"
  }

  case object Html extends Output {
    override val output: String = "html"
  }

  case object HtmlWithCss extends Output {
    override val output: String = "html"
    override val css: Boolean = true
  }

  final case class Config(
    displayMessages: Boolean = false,        // determines whether Message.Set() calls are logged
    displayErrors: Boolean = true,           // determines whether error messages are shown on the console
    undefinedCharError: Boolean = false,     // determines whether "unknown characters" (i.e., no glyph in the configured fonts) are saved in the error array
    extensions: String = "",                 // a convenience option to add MathJax extensions; example: 'Safe,TeX/noUndefined'
    fontURL: String = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/fonts/HTML-CSS", // for webfont urls in the CSS for HTML output
    font: String = "TeX"                     // possible values are TeX, STIX-Web, Asana-Math, Neo-Euler, Gyre-Pagella,
                                             // Gyre-Termes and Latin-Modern. Note that not all mathematical characters
                                             // are available in all fonts (e.g., Neo-Euler does not include italic
                                             // characters), so some mathematics may work better in some fonts than in
                                             // others. The STIX-Web font is the most complete.
    // paths: Map[String, String] = Map.empty,  // configures custom path variables (e.g., for third party extensions, cf. test/config-third-party-extensions.js)
  )

  def j2v8Version: String = "4.8.0"

  def j2v8dependency: String = s"com.eclipsesource.j2v8:j2v8_${getOS}_$getArchSuffix:$j2v8Version"

  private def getOS: String = {
    val osName: String = System.getProperty("os.name") + System.getProperty("java.specification.vendor")
    val isAndroid: Boolean = osName.contains("Android")

    if (osName.startsWith("Windows")) "win32"
    else if (osName.startsWith("Mac")) "macosx"
    else if (osName.startsWith("Linux") && !isAndroid) "linux"
    else if (isAndroid) "android"
    else throw new UnsatisfiedLinkError("Unsupported platform: " + osName)
  }

  private def getArchSuffix: String = System.getProperty("os.arch") match {
    case "i686" => "x86"
    case "amd64" => "x86_64"
    case "nacl" => "armv7l"
    case "aarch64" => "armv7l"
    case arch => arch
  }
}
