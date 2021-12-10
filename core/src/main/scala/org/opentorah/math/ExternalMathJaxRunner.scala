package org.opentorah.math

final class ExternalMathJaxRunner(
  node: Node,
  math: MathConfiguration
) extends MathJaxRunner(
  math
):

  override protected def typeset(
    options: Map[String, Matchable],
    outputName: String,
  ): String =  node.evaluate(math.mathJax.nodeSnippet(
    math,
    options,
    outputName
  ))
