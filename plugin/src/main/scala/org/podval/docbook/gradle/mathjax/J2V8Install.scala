package org.podval.docbook.gradle.mathjax

import java.io.File

import com.eclipsesource.v8.V8
import org.gradle.api.Project
import org.podval.docbook.gradle.util.Gradle
import org.podval.fop.mathjax.J2V8
import org.podval.fop.util.{Architecture, Os}

object J2V8Install {

  def load(project: Project, os: Os, arch: Architecture, into: File): Either[String, String] = {
    val dependencyNotationAndLibraryName: Option[(String, String)] =
      J2V8.dependencyNotationAndLibraryName(os, arch)

    if (dependencyNotationAndLibraryName.isEmpty) Left(s"No J2V8 distribution for $os on $arch") else {
      val (dependencyNotation: String, libraryName: String) = dependencyNotationAndLibraryName.get

      val artifact: Option[File] = try Some(Gradle.getArtifact(project, dependencyNotation)) catch {
        case _: IllegalStateException => None
      }

      if (artifact.isEmpty) Left(s"No J2V8 artifact $dependencyNotation") else {
        try {
          load(project, artifact.get, libraryName, into)

          Right(s"Loaded J2V8 library $dependencyNotation!$libraryName")
        } catch {
          case e: UnsatisfiedLinkError => Left(s"Failed to load J2V8 library: ${e.getMessage}")
        }
      }
    }
  }

  private def load(
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
