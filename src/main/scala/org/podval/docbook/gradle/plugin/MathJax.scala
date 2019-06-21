package org.podval.docbook.gradle.plugin

import java.io.File

import org.gradle.api.Project
import org.podval.docbook.gradle.{j2v8, mathjax, node}
import org.podval.docbook.gradle.util.{Architecture, Gradle, Logger, Os, Platform}

private class MathJax(project: Project, useJ2V8: Boolean, layout: Layout, logger: Logger) {
  val os: Os = Platform.getOs
  val arch: Architecture = Platform.getArch

  def getTypesetter(configuration: mathjax.Configuration): mathjax.Typesetter = {
    // make sure Node and MathJax are installed
    val nodeInstallation: node.Installation = installNode

    // If J2V8 is configured to be used, is available and actually loads - we use it;
    // otherwise each typesetting is done by calling Node in a separate process.
    val typesetterFactory: mathjax.Typesetter.Factory =
      if (useJ2V8 && loadJ2V8) j2v8.OneUseMathJaxTypesetterFactory
      else mathjax.ExternalTypesetterFactory

    typesetterFactory.get(nodeInstallation, configuration, logger)
  }

  private def installNode: node.Installation = {
    val version: String = node.Distribution.defaultVersion

    val distribution: node.Distribution = new node.Distribution(version, os, arch)

    val installation: node.Installation = new node.Installation(distribution, layout.nodeRoot)

    if (!installation.root.exists()) {
      logger.info(s"Installing $installation")

      val nodeArtifact: File = Gradle.getArtifact(
        project,
        distribution.dependencyNotation,
        newRepository = node.Distribution.repository
      )

      Gradle.unpack(
        project,
        archiveFile = nodeArtifact,
        isZip = distribution.isZip,
        into = installation.nodeRoot
      )

      installation.postInstall()
    }

    if (!installation.nodeModules.exists()) {
      logger.info(s"Installing mathjax-node")
      installation.npmInstall("mathjax-node")
    }

    installation
  }

  private def loadJ2V8: Boolean = {
    val dependencyNotationAndLibraryName: Option[(String, String)] =
      j2v8.J2V8.dependencyNotationAndLibraryName(os, arch)

    if (dependencyNotationAndLibraryName.isEmpty) {
      logger.warn(s"No J2V8 distribution for $os on $arch")
      false
    } else {
      val (dependencyNotation: String, libraryName: String) = dependencyNotationAndLibraryName.get

      val artifact: Option[File] = try Some(Gradle.getArtifact(project, dependencyNotation)) catch {
        case _: IllegalStateException => None
      }

      if (artifact.isEmpty) {
        logger.warn(s"No J2V8 artifact $dependencyNotation")
        false
      } else {
        try {
          val libraryDirectory: File = layout.j2v8LibraryDirectory

          libraryDirectory.mkdirs()

          Gradle.extract(
            project,
            zipFile = artifact.get,
            toExtract = libraryName,
            isDirectory = false,
            into = libraryDirectory
          )

          val libraryPath: String = new File(libraryDirectory, libraryName).getAbsolutePath

          logger.info(s"Loading J2V8 library $libraryPath")

          System.load(libraryPath)

          j2v8.J2V8.setNativeLibraryLoaded()

          true

        } catch {
          case e: UnsatisfiedLinkError =>
            logger.warn(s"Failed to load J2V8 library: ${e.getMessage}")
            false
        }
      }
    }
  }
}
