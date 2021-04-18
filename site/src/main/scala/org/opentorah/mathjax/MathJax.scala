package org.opentorah.mathjax

import org.opentorah.util.Json
import org.opentorah.xml.Xml

/*
  THE PLAN:
  - if J2V8 doesn't support running Node.js - remove it and all the supporting code;
  - adjust in MathJax3:
    - nodeConfiguration (to be removed with J2V8?);
    - htmlConfiguration;
    - exInEms;
    - packageName;
    - configurationFunction;
    - typesetFunction;
    - errorsArray;
    - optionsMap;
    - nodeSnippet;
  - what version of MathJax loads for MathJax2? do I need to specify the version?
  - verify that FOP works with both MathJax2 and MathJax3 for calendar (how does it even work? or does it?);
  - upgrade Node.js version;
  - in MathJaxConfiguration, adjust for versions 2 and 3:
    - fontURL;
    - fonts;
    - font checking;
    - defaultFont;
    - defaultFont;
    - inputs;
    - texExtensions.
 */
trait MathJax {

  def body(payload: Xml.Nodes): Seq[Xml.Element]

  final def htmlConfigurationString(configuration: MathJaxConfiguration): String =
    Json.fromMap(htmlConfiguration(configuration))

  def htmlConfiguration(configuration: MathJaxConfiguration): Map[String, Any]

  def nodeConfiguration(configuration: MathJaxConfiguration): Map[String, Any]

  val exInEms: Float

  val packageName: String

  val configurationFunction: String

  val typesetFunction: String

  val errorsArray: String

  def optionsMap(
    math: String,
    inputName: String,
    outputName: String,
    fontSize: Float
  ): Map[String, Any]

  def nodeSnippet(
    configuration: MathJaxConfiguration,
    options: Map[String, Any],
    outputName: String
  ): String
}

object MathJax {

  def get(useMathJax3: Boolean): MathJax = if (useMathJax3) MathJax3 else MathJax2

  private object MathJax2 extends MathJax {

    // TODO? <script async src="https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=TeX-AMS-MML_CHTML"/>
    override def body(payload: Xml.Nodes): Seq[Xml.Element] = Seq(
      <script type="text/javascript">window.MathJax={payload};</script>,
      <script type="text/javascript" src="https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=MML_HTMLorMML"/>
    )

    override def nodeConfiguration(configuration: MathJaxConfiguration): Map[String, Any] = Map(
      "displayMessages"     -> configuration.displayMessages,
      "displayErrors"       -> configuration.displayErrors,
      "undefinedCharError"  -> configuration.undefinedCharError,
      "extensions"          -> configuration.extensions.mkString(","),
      "fontURL"             -> configuration.fontURL,
      // standard MathJax configuration options; see https://docs.mathjax.org for more detail
      "MathJax" -> Map(
        "jax" -> (MathJaxConfiguration.inputs ++ List("output/SVG")),
        "TeX" -> Map("extensions" -> MathJaxConfiguration.texExtensions),
        "SVG" -> Map("font" -> configuration.font)
      )
    )

    override def htmlConfiguration(configuration: MathJaxConfiguration): Map[String, Any] = Map(
      "jax" -> (MathJaxConfiguration.inputs ++ List("output/CommonHTML", "output/HTML-CSS", "output/NativeMML", "output/SVG")),
      "extensions" -> List("tex2jax.js", "mml2jax.js", "asciimath2jax.js", "MathMenu.js", "MathZoom.js"),
      "tex2jax" -> Map(
        "processEscapes" -> configuration.processEscapes,
        "inlineMath" -> Delimiters.json(configuration.texInlineDelimiters),
        "displayMath" -> Delimiters.json(configuration.texDelimiters)
      ),
      "mml2jax" -> Map(),
      "asciimath2jax" -> Map("delimiters" -> Delimiters.json(configuration.asciiMathDelimiters)),
      "TeX" -> Map("extensions" -> MathJaxConfiguration.texExtensions),
      "MathML" -> Map(),
      "AsciiMath" -> Map(),
      "CommonHTML" -> Map(),
      "HTML-CSS" -> Map(),
      "NativeMML" -> Map(),
      "SVG" -> Map("font" -> configuration.font),
      "PreviewHTML" -> Map(),
      "PlainSource" -> Map()
    )

    /* Note:
Reading of the code that creates SVG and sets its sizes
(https://github.com/mathjax/MathJax/blob/master/unpacked/jax/output/SVG/jax.js)
made clear that:
- MathJax assumes ex height of 430.554 milli-ems (WTF?!), while Batik assumes ex height of 500 milli-ems,
 so before handing the SVG image to Batik, I need to convert viewport sizes to units that are interpreted
 the same way by MathJax and Batik: points (see Sizes.setViewPortSizes()).
*/
    override val exInEms: Float = 0.430554f

    override val packageName: String = "mathjax-node"

    override val configurationFunction: String = "config"

    override val typesetFunction: String = "typeset"

    override val errorsArray: String = "errors"

    override def optionsMap(
      math: String,
      inputName: String,
      outputName: String,
      fontSize: Float
    ): Map[String, Any] = Map(
      "useFontCache" -> true, // use <defs> and <use> in svg output ('true' by default)?
      "useGlobalCache" -> false, // use common <defs> for all equations?
      "linebreaks" -> false, // automatic linebreaking
      "speakText" -> false, // add textual alternative (for TeX/asciimath the input string, for MathML a dummy string)?
      "xmlns" -> "mml", // the namespace to use for MathML
      "timeout" -> 10 * 1000, // 10 second timeout before restarting MathJax
      "width" -> 100, // width of container (in ex) for linebreaking and tags
      "cjkCharWidth" -> 13, // width of CJK character
      "equationNumbers" -> "none", // automatic equation numbering ("none", "AMS" or "all")
      "ex" -> (fontSize * exInEms).toInt, // ex-size in pixels
      "format" -> inputName, // the input format (TeX, inline-TeX, AsciiMath, or MathML)
      "math" -> math, // the math string to typeset
      outputName -> true, // which output format to produce
      "css" -> false // generate CSS for HTML output?
    )

    // I have to use console.error() and not console.log() so that the output gets flushed before the project exist;
    // that is why I collect both out and err in Node.node()...
    override def nodeSnippet(
      configuration: MathJaxConfiguration,
      options: Map[String, Any],
      outputName: String
    ): String =
      s"""
         |var mjAPI = require("$packageName");
         |mjAPI.$configurationFunction(${Json.fromMap(nodeConfiguration(configuration))});
         |mjAPI.start();
         |mjAPI.$typesetFunction(${Json.fromMap(options)}, function (data) {
         |  if (!data.$errorsArray) { console.error(data.$outputName); }
         |});
         |""".stripMargin
  }


  // TODO https://docs.mathjax.org/en/latest/options/input/tex.html
  private object MathJax3 extends MathJax {

    override def body(payload: Xml.Nodes): Seq[Xml.Element] = Seq(
      <script
        src="https://polyfill.io/v3/polyfill.min.js?features=es6"/>,
      <script>MathJax = {payload};</script>,
      <script
        id="MathJax-script"
        async="async"
        src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"/>
    )

    override def nodeConfiguration(configuration: MathJaxConfiguration): Map[String, Any] = Map(
      // TODO !!!
    )

    override def htmlConfiguration(configuration: MathJaxConfiguration): Map[String, Any] = Map(
      // OLD
      //    "jax" -> (MathJaxConfiguration.inputs ++ List("output/CommonHTML", "output/HTML-CSS", "output/NativeMML", "output/SVG")),
      //    "extensions" -> List("tex2jax.js", "mml2jax.js", "asciimath2jax.js", "MathMenu.js", "MathZoom.js"),
      // NEW:
      //      loader: {
      //        load: ['[tex]/tagformat']
      //      },

      "tex" -> Map(
        // TODO?       packages: ['base'],        // extensions to use // MathJaxConfiguration.texExtensions)?
        "processEscapes" -> configuration.processEscapes,
        "inlineMath" -> Delimiters.json(configuration.texInlineDelimiters),
        "displayMath" -> Delimiters.json(configuration.texDelimiters)
      ),
    )

    // Looks like exFactor in MathJax3 chtml options is my `exInEms`; if so, it is now 0.5:
    override val exInEms: Float = 0.5f

    override val packageName: String = "mathjax-node"

    override val configurationFunction: String = "config"

    override val typesetFunction: String = "typeset"

    override val errorsArray: String = "errors"

    override def optionsMap(
      math: String,
      inputName: String,
      outputName: String,
      fontSize: Float
    ): Map[String, Any] = Map(
      "useFontCache" -> true, // use <defs> and <use> in svg output ('true' by default)?
      "useGlobalCache" -> false, // use common <defs> for all equations?
      "linebreaks" -> false, // automatic linebreaking
      "speakText" -> false, // add textual alternative (for TeX/asciimath the input string, for MathML a dummy string)?
      "xmlns" -> "mml", // the namespace to use for MathML
      "timeout" -> 10 * 1000, // 10 second timeout before restarting MathJax
      "width" -> 100, // width of container (in ex) for linebreaking and tags
      "cjkCharWidth" -> 13, // width of CJK character
      "equationNumbers" -> "none", // automatic equation numbering ("none", "AMS" or "all")
      "ex" -> (fontSize * exInEms).toInt, // ex-size in pixels
      "format" -> inputName, // the input format (TeX, inline-TeX, AsciiMath, or MathML)
      "math" -> math, // the math string to typeset
      outputName -> true, // which output format to produce
      "css" -> false // generate CSS for HTML output?
    )

    // I have to use console.error() and not console.log() so that the output gets flushed before the project exist;
    // that is why I collect both out and err in Node.node()...
    override def nodeSnippet(
      configuration: MathJaxConfiguration,
      options: Map[String, Any],
      outputName: String
    ): String =
      s"""
         |var mjAPI = require("$packageName");
         |mjAPI.$configurationFunction(${Json.fromMap(nodeConfiguration(configuration))});
         |mjAPI.start();
         |mjAPI.$typesetFunction(${Json.fromMap(options)}, function (data) {
         |  if (!data.$errorsArray) { console.error(data.$outputName); }
         |});
         |""".stripMargin
  }
}
