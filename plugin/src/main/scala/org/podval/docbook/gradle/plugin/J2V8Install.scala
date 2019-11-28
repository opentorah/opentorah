package org.podval.docbook.gradle.plugin

import java.io.File

import com.eclipsesource.v8.V8
import org.gradle.api.Project
import org.podval.fop.mathjax.J2V8Distribution
import org.podval.fop.util.{Architecture, Logger, Os}

object J2V8Install {

  def install(project: Project, os: Os, arch: Architecture, into: File, logger: Logger): Either[String, String] = {
    val j2v8Distribution: J2V8Distribution = new J2V8Distribution(os, arch)

    if (j2v8Distribution.version.isEmpty) Left(s"No J2V8 distribution for $os on $arch") else {
      val dependencyNotation: String = j2v8Distribution.dependencyNotation
      val libraryName: String = j2v8Distribution.libraryName

      val artifact: Option[File] = try Some(Gradle.getArtifact(project, dependencyNotation)) catch {
        case _: IllegalStateException => None
      }

      if (artifact.isEmpty) Left(s"No J2V8 artifact $dependencyNotation") else {
        try {
          install(project, artifact.get, libraryName, into)

          Right(s"Loaded J2V8 library $dependencyNotation!$libraryName")
        } catch {
          case e: UnsatisfiedLinkError => Left(s"Failed to load J2V8 library: ${e.getMessage}")
        }
      }
    }
  }

  private def install(
    project: Project,
    artifact: File,
    libraryName: String,
    into: File
  ): Unit = {
    into.mkdirs()

    Gradle.extract(
      project,
      zipFile = artifact,
      toExtract = libraryName,
      isDirectory = false,
      into
    )

    val libraryPath: String = new File(into, libraryName).getAbsolutePath

    System.load(libraryPath)

    setNativeLibraryLoaded()
  }

  private def setNativeLibraryLoaded(): Unit = {
    val field = classOf[V8].getDeclaredField("nativeLibraryLoaded")
    field.setAccessible(true)
    field.set(null, true)
  }
}
