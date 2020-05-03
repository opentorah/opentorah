package org.opentorah.node

import java.io.File
import java.nio.file.{Files, Path, Paths}
import org.gradle.api.Project
import org.opentorah.util.Gradle
import org.slf4j.{Logger, LoggerFactory}

final class NodeFromArtifact(
  into: File,
  distribution: NodeDistribution,
  nodeModulesParent: File,
) extends Node(
  nodeModulesParent,
  nodeExec = distribution.nodeExec(into),
  npmExec = distribution.npmExec(into)
) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[NodeFromArtifact])

  override def toString: String = s"Node in ${distribution.root(into)} with $nodeExec and $npmExec with modules in $nodeModules"

  def install(project: Project, overwrite: Boolean): Unit = {
    if (!overwrite && this.exists) logger.info(s"Existing installation detected: $this") else {
      logger.info(s"Installing $this")

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
        into
      )

      logger.info(s"Extracted $distribution into $into")

      fixNpmSymlink()
    }
  }

  private def fixNpmSymlink(): Unit = if (!distribution.isWindows) {
    val npm: Path = npmExec.toPath
    val deleted: Boolean = Files.deleteIfExists(npm)
    if (deleted) {
      val npmCliJs: String = new File(distribution.root(into), s"lib/node_modules/npm/bin/npm-cli.js").getAbsolutePath
      Files.createSymbolicLink(
        npm,
        distribution.bin(into).toPath.relativize(Paths.get(npmCliJs))
      )
    }
  }
}
