package org.podval.docbook.gradle.mathjax

import java.io.File

import com.eclipsesource.v8.{NodeJS, V8, V8Array, V8Function, V8Object}

final class MathJax(nodeModulesRoot: File) {

  MathJax.loadV8()

  private val nodeJS: NodeJS = NodeJS.createNodeJS()
  private val v8: V8 = nodeJS.getRuntime
  private val mathJaxNode: V8Object = nodeJS.require(new File(nodeModulesRoot, "node_modules/mathjax-node"))

  def open(): Unit = {
    config()
    start()
  }

  def config(): Unit = {
    val config: V8Object = new V8Object(v8)
    val mathJax: V8Object = new V8Object(v8)
    val svgConfig: V8Object = new V8Object(v8)
    svgConfig.add("font", "STIX-Web")
//    mathJax.add("SVG", svgConfig)
    config.add("MathJax", mathJax)

//    Example:
//    {
//      MathJax: {SVG: {font: "STIX-Web"}},
//      displayErrors: false,
//      extensions: 'Safe,TeX/noUndefined'
//    }

    val configArgs = new V8Array(v8)
    configArgs.push(config)
    mathJaxNode.executeVoidFunction("config", configArgs)
    svgConfig.release()
    mathJax.release()
    config.release()
    configArgs.release()
  }

  def start(): Unit = {
    val startArgs = new V8Array(v8)
    mathJaxNode.executeVoidFunction("start", startArgs)
    startArgs.release()
  }

  def typeset2String(math: String, input: MathJax.Input, output: MathJax.Output): String = {
    val data: V8Object = typeset(math, input.input, output.output, output.css)
    val result: String = data.getString(output.output)
    data.release()
    result
  }

  def typeset2Dom(math: String, input: MathJax.Input, output: MathJax.Output): V8Object = {
    val nodeOutput: String = output.output + "Node"
    val data: V8Object = typeset(math, input.input, nodeOutput, output.css)
    val result: V8Object = data.getObject(nodeOutput)
    data.release()
    result
  }

  private def typeset(math: String, input: String, output: String, css: Boolean): V8Object = {
    var result: V8Object = null

    val typesetCallback: V8Function = new V8Function(v8, (_: V8Object, parameters: V8Array) => {
      result = parameters.getObject(0)
      null
    })

    val typeset: V8Object = new V8Object(v8)
    typeset.add("format", input)
    typeset.add(output, true)
    typeset.add("css", css)
    typeset.add("speakText", false)

    typeset.add("math", math)
    val typesetArgs = new V8Array(v8)
    typesetArgs.push(typeset)
    typesetArgs.push(typesetCallback)
    mathJaxNode.executeObjectFunction("typeset", typesetArgs)

    typeset.release()
    typesetCallback.release()
    typesetArgs.release()

    while (nodeJS.isRunning) {
      nodeJS.handleMessage()
    }

    result
  }

  def close() {
    mathJaxNode.release()
    nodeJS.release()
  }
}

object MathJax {
  private def libraryExtractDirectory: File = new File("/tmp/j2v8/")
  private var loadedV8: Boolean = false
  def loadV8(): Unit = {
    if (!loadedV8) {
      loadedV8 = true
      val tmpDirectory: File = libraryExtractDirectory
      tmpDirectory.mkdirs()
      V8.createV8Runtime("dummy", tmpDirectory.getAbsolutePath).release()
    }
  }

  sealed trait Input {
    def input: String
  }

  sealed trait Output {
    def output: String
    def css: Boolean = false
  }

  case object Tex extends Input {
    override def input: String = "TeX"
  }

  case object TexInline extends Input {
    override def input: String = "inline-TeX"
  }

  case object AsciiMath extends Input {
    override def input: String = "AsciiMath"
  }

  case object MathML extends Input with Output {
    override def input: String = "MathML"
    override def output: String = "mml"
  }

  // For SVG, width, height and style attributes are repeated in separate keys of the returned data.
  case object Svg extends Output {
    override def output: String = "svg"
  }

  case object Html extends Output {
    override def output: String = "html"
  }

  // TODO output is the same as for Html?
  case object HtmlWithCss extends Output {
    override def output: String = "html"
    override def css: Boolean = true
  }

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
