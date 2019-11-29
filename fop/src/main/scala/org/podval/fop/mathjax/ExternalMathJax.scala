package org.podval.fop.mathjax

import org.podval.fop.util.{Json, Logger}

object ExternalMathJax extends MathJax.Factory {

  override def get(
    node: Node,
    configuration: Configuration,
    logger: Logger
  ): MathJax = new MathJax(node, configuration, logger) {

    override protected def typeset(
      options: Map[String, Any],
      outputName: String,
    ): String =  node.evaluate(
      // I have to use console.error() and not console.log() so that the output gets flushed before the project exist;
      // that is why I collect both out and err in Node.node()...
      s"""
         |var mjAPI = require("mathjax-node");
         |mjAPI.config(${Json.fromMap(configuration.toMap)});
         |mjAPI.start();
         |mjAPI.typeset(${Json.fromMap(options)}, function (data) {
         |  if (!data.errors) { console.error(data.$outputName); }
         |});
         """.stripMargin
    )
  }
}
