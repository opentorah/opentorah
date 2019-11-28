package org.podval.docbook.gradle.mathjax

import java.io.File

import org.gradle.api.Project
import org.podval.docbook.gradle.util.Gradle
import org.podval.fop.mathjax.{Node, NodeDistribution}
import org.podval.fop.util.{Architecture, Logger, Os}

object NodeInstall {
  val defaultVersion: String = "10.15.3"

  def install(project: Project, os: Os, arch: Architecture, into: File, logger: Logger): Node = {
    val version: String = defaultVersion

    val distribution: NodeDistribution = new NodeDistribution(version, os, arch)

    val installation: Node = new Node(distribution, into)

    if (!installation.root.exists()) {
      logger.info(s"Installing $installation")

      val nodeArtifact: File = Gradle.getArtifact(
        project,
        repositoryUrl = "https://nodejs.org/dist",
        artifactPattern = "v[revision]/[artifact](-v[revision]-[classifier]).[ext]",
        ivy = "v[revision]/ivy.xml",
        distribution.dependencyNotation
      )

      Gradle.unpack(
        project,
        archiveFile = nodeArtifact,
        isZip = distribution.isZip,
        into = into
      )

      installation.postInstall()
    }

    if (!installation.nodeModules.exists()) {
      logger.info(s"Installing mathjax-node")
      installation.npmInstall("mathjax-node")
    }

    installation
  }
}
