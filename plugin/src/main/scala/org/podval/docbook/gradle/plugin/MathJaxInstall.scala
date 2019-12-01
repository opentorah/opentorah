package org.podval.docbook.gradle.plugin

import java.io.File

import org.gradle.api.Project
import org.podval.fop.mathjax.{J2V8, J2V8Distribution, Node, NodeDistribution, NodeFromArtifact}
import org.podval.fop.util.Logger

object MathJaxInstall {

  def installNode(project: Project, distribution: NodeDistribution, into: File, logger: Logger): Node = {
    val node: NodeFromArtifact = NodeFromArtifact(distribution, into)

    if (node.root.exists) {
      logger.info(s"Existing installation detected: $node")
    } else {
      logger.info(s"Installing $node")

      val artifact: File = Gradle.getArtifact(
        project,
        repositoryUrl = "https://nodejs.org/dist",
        artifactPattern = "v[revision]/[artifact](-v[revision]-[classifier]).[ext]",
        ivy = "v[revision]/ivy.xml",
        dependencyNotation = distribution.dependencyNotation
      )

      logger.info(s"Resolved $distribution: $artifact")

      Gradle.unpack(
        project,
        archiveFile = artifact,
        isZip = distribution.isZip,
        into = into
      )

      logger.info(s"Extracted $distribution into $into")

      node.postInstall()
    }

    node
  }

  def installJ2V8(project: Project, distribution: J2V8Distribution, into: File, logger: Logger): Option[J2V8] =
    if (distribution.version.isEmpty) {
      logger.warn(s"No $distribution")
      None
    } else {
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
