package org.opentorah.math

import org.opentorah.util.Json
import org.opentorah.xml.ScalaXml

object MathJax2 extends MathJax:

  // TODO use TeX-MML-AM_CHTML instead of TeX-MML-AM_SVG?
  override def body(payload: ScalaXml.Nodes): Seq[ScalaXml.Element] = Seq(
    <script type="text/javascript">window.MathJax={payload};</script>,
    <script type="text/javascript" src="https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=TeX-MML-AM_SVG"/>
  )

  override def htmlConfiguration(math: MathConfiguration): Map[String, Matchable] = Map(
    // see https://docs.mathjax.org/en/v2.7-latest/options/index.html
    "tex2jax"       -> math.texConfiguration,
    "asciimath2jax" -> Map("delimiters" -> Delimiters.json(math.asciiMathDelimiters)),
    "SVG"           -> Map("font" -> math.font.get)
  )

  override val npmPackagesToInstall: List[String] = List("mathjax-node")

  override val useEsm: Boolean = false

  // Note: see Sizes for notes ;)
  private val exInEms: Float = 0.430554f

  override def nodeScript(
    math: MathConfiguration,
    mathString: String,
    input: Input,
    fontSize: Float
  ): String =

    // Note: only non-defaulted options are set;
    // for the full list, see https://github.com/mathjax/MathJax-node
    val configuration: Map[String, Matchable] = Map(
      // standard MathJax configuration options; see https://docs.mathjax.org/en/v2.7-latest/options/index.html
      "MathJax" -> Map(
        "jax"        -> List("input/TeX", "input/AsciiMath", "input/MathML", "output/SVG"),
        "extensions" -> List("tex2jax.js", "mml2jax.js", "asciimath2jax.js"),
        "tex2jax"    -> Map("processEscapes" -> math.processEscapes.contains(true)),
        "TeX"        -> Map("extensions" -> List("AMSmath.js", "AMSsymbols.js", "noErrors.js", "noUndefined.js")),
        "SVG"        -> Map("font" -> math.font.get)
      )
    )

    // Note: see https://github.com/mathjax/MathJax-node
    val options: Map[String, Matchable] = Map(
      "math"      -> mathString, // the math string to typeset
      "svg"       -> true, // which output format to produce,
      "format"    -> input.name, // the input format (TeX, inline-TeX, AsciiMath, or MathML)
      "ex"        -> (fontSize * exInEms).toInt, // ex-size in pixels
      "speakText" -> false // add textual alternative (for TeX/asciimath the input string, for MathML a dummy string)?
    )

    // For SVG, width, height and style attributes are repeated in separate keys of the returned data.

    s"""
       |var mjAPI = require("mathjax-node");
       |mjAPI.config(${Json.fromMap(configuration)});
       |mjAPI.start();
       |mjAPI.typeset(${Json.fromMap(options)}, function (data) {
       |  if (!data.errors) { console.log(data.svg); }
       |});
       |""".stripMargin
