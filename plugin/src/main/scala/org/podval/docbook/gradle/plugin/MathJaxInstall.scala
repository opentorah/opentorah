package org.podval.docbook.gradle.plugin

import java.io.File

import org.gradle.api.Project
import org.podval.fop.mathjax.{Configuration, ExternalMathJax, J2V8, J2V8Distribution, J2V8MathJax, MathJax, Node, NodeDistribution}
import org.podval.fop.util.{Architecture, Logger, Os, Platform}

object MathJaxInstall {
  val nodeVersion: String = "10.15.3"

  def install(
    configuration: Configuration,
    useJ2V8: Boolean,
    project: Project,
    layout: Layout,
    logger: Logger
  ): MathJax = {
    val os: Os = Platform.getOs
    val arch: Architecture = Platform.getArch

    val node: Node = new Node(new NodeDistribution(nodeVersion, os, arch), layout.nodeRoot)

    // Make sure Node is installed
    if (!node.root.exists()) {
      logger.info(s"Installing $node")

      installNode(
        project,
        installation = node,
        into = layout.nodeRoot,
        logger
      )
    }

    // Make sure MathJax is installed
    if (!node.nodeModules.exists()) {
      logger.info(s"Installing mathjax-node")
      node.npmInstall("mathjax-node")
    }

    // If J2V8 is configured to be used, is available and actually loads - we use it;
    // otherwise each typesetting is done by calling Node in a separate process.
    val reallyUseJ2V8: Boolean = useJ2V8 &&  {
      val distribution: J2V8Distribution = new J2V8Distribution(os, arch)

      if (distribution.version.isEmpty) {
        logger.warn(s"No $distribution")
        false
      } else {
        val j2v8: Option[J2V8] = installJ2V8(
          project,
          distribution,
          into = layout.j2v8LibraryDirectory,
          logger
        )
        j2v8.isDefined && j2v8.get.load(logger)
      }
    }

    if (reallyUseJ2V8) new J2V8MathJax(node, configuration, logger)
    else new ExternalMathJax(node, configuration, logger)
  }

  private def installNode(project: Project, installation: Node, into: File, logger: Logger): Unit = {
    val dependencyNotation: String = installation.distribution.dependencyNotation

    val artifact: File = Gradle.getArtifact(
      project,
      repositoryUrl = "https://nodejs.org/dist",
      artifactPattern = "v[revision]/[artifact](-v[revision]-[classifier]).[ext]",
      ivy = "v[revision]/ivy.xml",
      dependencyNotation
    )

    logger.info(s"Resolved $installation artifact $dependencyNotation: $artifact")

    Gradle.unpack(
      project,
      archiveFile = artifact,
      isZip = installation.distribution.isZip,
      into = into
    )

    logger.info(s"Extracted $installation into $into")

    installation.postInstall()
  }

  def installJ2V8(project: Project, distribution: J2V8Distribution, into: File, logger: Logger): Option[J2V8] = {
    val dependencyNotation: String = distribution.dependencyNotation
    val libraryName: String = distribution.libraryName

    val artifact: Option[File] = try Some(Gradle.getArtifact(project, dependencyNotation)) catch {
      case _: IllegalStateException => None
    }

    artifact.fold[Option[J2V8]] {
      logger.warn(s"No $distribution artifact $dependencyNotation")
      None
    } { artifact =>
      logger.info(s"Resolved $distribution artifact $dependencyNotation: $artifact")

      into.mkdirs()

      Gradle.extract(
        project,
        zipFile = artifact,
        toExtract = libraryName,
        isDirectory = false,
        into
      )

      logger.info(s"Extracted $distribution ($dependencyNotation!$libraryName) into $into")

      val libraryPath: String = new File(into, libraryName).getAbsolutePath
      Some(new J2V8(libraryPath))
    }
  }
}
