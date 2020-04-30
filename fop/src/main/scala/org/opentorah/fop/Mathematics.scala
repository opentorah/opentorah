package org.opentorah.fop

import java.io.File
import org.gradle.api.Project
import org.opentorah.fop.gradle.MathJaxInstall
import org.opentorah.fop.mathjax.{Configuration, ExternalMathJax, J2V8, J2V8MathJax, MathJax, Node}

object Mathematics {

  def getMathJax(
    project: Project,
    nodeParent: File,
    overwriteNode: Boolean,
    nodeModulesParent: File,
    overwriteMathJax: Boolean,
    j2v8Parent: Option[File],
    configuration: Configuration
  ): MathJax = getMathJax(
    node = MathJaxInstall.installNode(
      project,
      into = nodeParent,
      overwrite = overwriteNode,
      nodeModulesParent),
    overwriteMathJax,
    j2v8 = j2v8Parent.flatMap(j2v8Parent => MathJaxInstall.installJ2V8(
      project,
      into = j2v8Parent)),
    configuration
  )

  def getMathJax(
    nodeModulesParent: File,
    overwriteMathJax: Boolean,
    configuration: Configuration = new Configuration
  ): MathJax = getMathJax(
    node = Node.fromOs(nodeModulesParent).get,
    overwriteMathJax,
    j2v8 = None,
    configuration
  )

  def getMathJax(
    node: Node,
    overwriteMathJax: Boolean,
    j2v8: Option[J2V8],
    configuration: Configuration
  ): MathJax = {
    node.installMathJax(overwriteMathJax)

    // If J2V8 is configured to be used, is available and actually loads - we use it;
    // otherwise each typesetting is done by calling Node in a separate process.
    val useJ2V8: Boolean = j2v8.isDefined && j2v8.get.load()
    if (useJ2V8) new J2V8MathJax(node, configuration)
    else new ExternalMathJax(node, configuration)
  }
}
