package org.opentorah.mathjax

import org.opentorah.node.Node
import org.opentorah.util.Json

final class ExternalMathJax(
  node: Node,
  configuration: Configuration
) extends MathJax(configuration) {

  override protected def typeset(
    options: Map[String, Any],
    outputName: String,
  ): String =  node.evaluate(
    // I have to use console.error() and not console.log() so that the output gets flushed before the project exist;
    // that is why I collect both out and err in Node.node()...
    s"""
       |var mjAPI = require("${MathJax.packageName}");
       |mjAPI.config(${Json.fromMap(configuration.toMap)});
       |mjAPI.start();
       |mjAPI.typeset(${Json.fromMap(options)}, function (data) {
       |  if (!data.errors) { console.error(data.$outputName); }
       |});
       |""".stripMargin
  )
}
