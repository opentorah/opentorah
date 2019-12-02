package org.podval.fop

import java.io.File

import org.podval.fop.mathjax.{Configuration, ExternalMathJax, J2V8, J2V8MathJax, MathJax, MathJaxFopPlugin, Node,
  NodeFromOs}
import org.podval.fop.util.Logger

object Mathematics {

  def getMathJax(
    node: Node,
    overwriteMathJax: Boolean,
    j2v8: Option[J2V8],
    configuration: Configuration,
    logger: Logger
  ): MathJax = {
    node.installMathJax(overwriteMathJax, logger)

    // If J2V8 is configured to be used, is available and actually loads - we use it;
    // otherwise each typesetting is done by calling Node in a separate process.
    val useJ2V8: Boolean = j2v8.isDefined && j2v8.get.load(logger)
    if (useJ2V8) new J2V8MathJax(node, configuration, logger)
    else new ExternalMathJax(node, configuration, logger)
  }

  def getFopPlugin(
     nodeModulesParent: File,
     overwriteMathJax: Boolean,
     configuration: Configuration = new Configuration,
     logger: Logger
  ): FopPlugin = new MathJaxFopPlugin(
    getMathJax(
      NodeFromOs.apply(nodeModulesParent).get,
      overwriteMathJax,
      None,
      configuration,
      logger)
  )
}
