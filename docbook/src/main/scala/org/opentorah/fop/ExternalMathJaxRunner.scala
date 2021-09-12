package org.opentorah.fop

import org.opentorah.mathjax.{MathJax, MathJaxConfiguration}

final class ExternalMathJaxRunner(
  node: Node,
  mathJax: MathJax,
  configuration: MathJaxConfiguration
) extends MathJaxRunner(
  mathJax,
  configuration
):

  override protected def typeset(
    options: Map[String, Matchable],
    outputName: String,
  ): String =  node.evaluate(mathJax.nodeSnippet(
    configuration,
    options,
    outputName
  ))
