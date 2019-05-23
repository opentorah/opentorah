package org.podval.docbook.gradle.mathjax

import java.io.{File, InputStream, StringBufferInputStream}
import java.nio.file.Files

import com.eclipsesource.v8.{NodeJS, V8, V8Array, V8Function, V8Object}
import com.eclipsesource.v8.utils.V8ObjectUtils
import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.fop.image.loader.batik.PreloaderSVG
import org.apache.fop.util.UnclosableInputStream
import org.podval.docbook.gradle.xml.Xml
import org.w3c.dom.Document
import org.w3c.dom.svg.SVGDocument

import scala.collection.JavaConverters._

final class MathJax(nodeModulesRoot: File) {

  private def v8: V8 = nodeJS.getRuntime

  private val nodeJS: NodeJS = {
    MathJax.loadV8()
    NodeJS.createNodeJS()
  }

  private val mathJaxNode: V8Object =
    nodeJS.require(new File(nodeModulesRoot, "node_modules/mathjax-node"))

  def close() {
    mathJaxNode.release()
    nodeJS.release()
  }

  def configure(configuration: MathJaxConfiguration): Unit = {
    val args: V8Array = V8ObjectUtils.toV8Array(v8, List(configuration.toMap).asJava)
    mathJaxNode.executeVoidFunction("config", args)
    args.release()
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

  // This is done automatically when typeset is first called.
//  private def start(): Unit = {
//    val args = new V8Array(v8)
//    mathJaxNode.executeVoidFunction("start", args)
//    args.release()
//  }

  def typeset(mathMLDocument: Document): SVGDocument = {
    val isInline: Boolean = DisplayAttribute.getWithDefault(mathMLDocument)
    val mode: MathJax.Input = ModeAttribute.getWithDefault(mathMLDocument)
    val input: MathJax.Input = if (isInline && (mode == MathJax.Tex)) MathJax.TexInline else mode

    typeset(
      what = if (input == MathJax.MathML) Xml.toString(mathMLDocument) else MathReader.unwrap(mathMLDocument),
      input = input,
      fontSize = FontSizeAttribute.doGet(mathMLDocument)
    )
  }

  def typeset(what: String, input: MathJax.Input, fontSize: Float): SVGDocument = {
    val svg: String = typeset2String(what, input, MathJax.Svg, fontSize.toInt)

    val in: InputStream = new UnclosableInputStream(new StringBufferInputStream(svg))
    val length: Int = in.available()
    in.mark(length + 1)
    val result: SVGDocument = MathJax.svgFactory.createSVGDocument(null, in)

    // set font size on the resulting SVG - it is needed for the sizes calculations:
    FontSizeAttribute.set(fontSize, result)

    result
  }

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

  private val inputs: Set[Input] = Set(Tex, AsciiMath, MathML) // Tex and TexInline are distinguished by DisplayAttribute

  def inputForName(name: String): Input = inputs.find(_.input == name).get

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

  private val svgFactory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(PreloaderSVG.getParserName)
}
