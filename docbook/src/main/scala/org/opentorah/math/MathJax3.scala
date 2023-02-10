package org.opentorah.math

import org.opentorah.util.{Json, Strings}
import org.opentorah.xml.ScalaXml

object MathJax3 extends MathJax:

  // see https://docs.mathjax.org/en/latest/options/input/index.html

  override def body(payload: ScalaXml.Nodes): Seq[ScalaXml.Element] = Seq(
    <script>MathJax = {payload};</script>,
    <script
      type="text/javascript"
      id="MathJax-script"
      async="async"
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"/>
  )

  override def htmlConfiguration(math: MathConfiguration): Map[String, Matchable] = Map(
    "loader" -> Map("load" -> List("input/asciimath")),
    "tex"  -> math.texConfiguration
  )

  override val npmPackagesToInstall: List[String] = List("mathjax-full@3")

  override val useEsm: Boolean = true

  // TODO
  override def nodeScript(
    math: MathConfiguration,
    mathString: String,
    input: Input,
    fontSize: Float
  ): String =

    val (inputComponent: String, promiseOf: String, additionalComponents: List[String]) = input.inputType match
      case Input.Type.Tex       => ("tex"      , "tex"      , List())
      case Input.Type.AsciiMath => ("asciimath", "asciimath", List())
      case Input.Type.MathML    => ("mml"      , "mathml"   , List("input/mml/entities"))

    val components: Seq[String] = List(
      s"input/$inputComponent",
      "output/svg",
      "adaptors/liteDOM"
    ) ++
      additionalComponents

    val configuration: Map[String, Matchable] = Map(
      "startup" -> Map("typeset"            -> false),
      "loader"  -> Map("load"               -> components),
      "options" -> Map("enableAssistiveMml" -> false),
      "tex"     -> Map("packages"           -> List("base", "autoload", "require", "ams", "newcommand", "configmacros", "noundefined"))
    )

    val em: Int = fontSize.toInt
    val ex: Int = (fontSize/2).toInt

    val options: Map[String, Matchable] = Map(
      "display"        -> Input.Display.isBlock(input.display),
      "em"             -> em,
      "ex"             -> ex,
      "containerWidth" -> 80 * ex
    )

    // TODO Why is it that unless I use "mathjax-full" instead of "mathjax", "esm" module is not found?
    // TODO Why is it that unless I nest the phases instead of chaining tme (as I think I saw in some examples), it breaks?
    s"""
       |require("mathjax-full").init(${Json.fromMap(configuration)}).then((MathJax) => {
       |  MathJax.${promiseOf}2svgPromise("${Strings.escape(mathString)}", ${Json.fromMap(options)})
       |  .then((node) => { console.log(MathJax.startup.adaptor.innerHTML(node)); });
       |}).catch(err => console.log(err));
       |""".stripMargin
