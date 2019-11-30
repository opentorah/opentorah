package org.podval.docbook.gradle.plugin

import java.io.File

import org.gradle.api.Project
import org.podval.fop.mathjax.{J2V8, J2V8Distribution}
import org.podval.fop.util.{Architecture, Logger, Os}

object J2V8Install {

  def install(project: Project, os: Os, arch: Architecture, into: File, logger: Logger): Boolean = {
    val j2v8Distribution: J2V8Distribution = new J2V8Distribution(os, arch)

    if (j2v8Distribution.version.isEmpty) {
      logger.warn(s"No J2V8 distribution for $os on $arch")
      false
    } else {
      val dependencyNotation: String = j2v8Distribution.dependencyNotation
      val libraryName: String = j2v8Distribution.libraryName

      val artifact: Option[File] = try Some(Gradle.getArtifact(project, dependencyNotation)) catch {
        case _: IllegalStateException => None
      }

      artifact.fold{
        logger.warn(s"No J2V8 artifact $dependencyNotation")
        false
      } { artifact =>
        into.mkdirs()

        Gradle.extract(
          project,
          zipFile = artifact,
          toExtract = libraryName,
          isDirectory = false,
          into
        )

        logger.info(s"Extracted J2V8 library $dependencyNotation!$libraryName")

        val libraryPath: String = new File(into, libraryName).getAbsolutePath
        val j2v8: J2V8 = new J2V8(libraryPath)
        j2v8.load(logger)

        true
      }
    }
  }
}
