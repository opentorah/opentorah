package org.opentorah.math

import org.opentorah.node.Node

final class ExternalMathJaxRunner(
  node: Node,
  math: MathConfiguration
) extends MathJaxRunner:

  override protected def typeset(
    mathString: String,
    input: Input,
    fontSize: Float
  ): String = node.evaluate(
    useEsm = math.mathJax.useEsm,
    script = math.mathJax.nodeScript(
      math,
      mathString,
      input,
      fontSize
    ))
