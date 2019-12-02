package org.podval.fop

import java.io.File

import org.gradle.api.Project
import org.podval.fop.gradle.MathJaxInstall
import org.podval.fop.mathjax.{Configuration, ExternalMathJax, J2V8, J2V8MathJax, MathJax, Node}
import org.podval.fop.util.Logger

object Mathematics {

  def getMathJax(
    project: Project,
    nodeParent: File,
    overwriteNode: Boolean,
    nodeModulesParent: File,
    overwriteMathJax: Boolean,
    j2v8Parent: Option[File],
    configuration: Configuration,
    logger: Logger
  ): MathJax = getMathJax(
    node = MathJaxInstall.installNode(
      project,
      into = nodeParent,
      overwrite = overwriteNode,
      nodeModulesParent,
      logger),
    overwriteMathJax,
    j2v8 = j2v8Parent.flatMap(j2v8Parent => MathJaxInstall.installJ2V8(
      project,
      into = j2v8Parent,
      logger)),
    configuration,
    logger
  )

  def getMathJax(
    nodeModulesParent: File,
    overwriteMathJax: Boolean,
    configuration: Configuration = new Configuration,
    logger: Logger
  ): MathJax = getMathJax(
    node = Node.fromOs(nodeModulesParent).get,
    overwriteMathJax,
    j2v8 = None,
    configuration,
    logger
  )

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
}
