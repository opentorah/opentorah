package org.podval.docbook.gradle.mathjax

import java.io.File

import org.gradle.api.Project
import org.podval.fop.mathjax.{Configuration, MathJax, Node}
import org.podval.fop.util.{Architecture, Logger, Os}

object MathJaxInstall {
  def get(
    project: Project,
    os: Os,
    arch: Architecture,
    nodeRoot: File,
    useJ2V8: Boolean,
    j2v8LibraryDirectory: File,
    configuration: Configuration,
    logger: Logger
  ): MathJax = {
    // make sure Node and MathJax are installed
    val node: Node = NodeInstall.install(project, os, arch, nodeRoot, logger)

    // If J2V8 is configured to be used, is available and actually loads - we use it;
    // otherwise each typesetting is done by calling Node in a separate process.
    val reallyUseJ2V8: Boolean = useJ2V8 && {
      val result: Either[String, String] = J2V8.load(project, os, arch, j2v8LibraryDirectory)
      result.fold(logger.warn, logger.info)
      result.isRight
    }

    val mathJaxFactory: MathJax.Factory = if (reallyUseJ2V8) J2V8MathJax else ExternalMathJax

    mathJaxFactory.get(node, configuration, logger)
  }
}
