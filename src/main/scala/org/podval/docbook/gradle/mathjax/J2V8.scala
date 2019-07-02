package org.podval.docbook.gradle.mathjax

import java.io.File

import com.eclipsesource.v8.V8
import org.gradle.api.Project
import org.podval.docbook.gradle.util.{Architecture, Gradle, Os}

import scala.jdk.CollectionConverters._

object J2V8 {

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

  private def dependencyNotationAndLibraryName(os: Os, arch: Architecture): Option[(String, String)] = {
    val version: Option[String] = os match {
      case Os.Windows | Os.Mac => Some("4.6.0")
      // Note: native library needs to be compatible with the Java code used by the plugin (see build.gradle),
      // so it should probably be 4.6.0 even for Linux, but version of Node in it doesn't work with mathjax-node:
      // mathjax-node/lib/main.js:163: SyntaxError:
      //   Block-scoped declarations (let, const, function, class) not yet supported outside strict mode
      //   for (let key in paths) {
      // Conclusion: I have to use 4.8.0 on Linux and in build.gradle, so this probably won't work on any other platform :(
      case Os.Linux => Some("4.8.0")
      case _ => None
    }

    version.map { version =>
      val osName: String = os match {
        case Os.Windows => "win32"
        case Os.Mac => "macosx"
        case Os.Linux => "linux"
        case _ => throw new IllegalArgumentException
      }

      val archName: String = arch match {
        case Architecture.i686 => "x86"
        case Architecture.x86_64 => "x86_64"
        case Architecture.amd64 => "x86_64"
        case _ => throw new IllegalArgumentException
      }

      (
        s"com.eclipsesource.j2v8:j2v8_${osName}_$archName:$version",
        s"libj2v8_${osName}_$archName.${os.libraryExtension}"
      )
    }
  }

  private def setNativeLibraryLoaded(): Unit = {
    val field = classOf[V8].getDeclaredField("nativeLibraryLoaded")
    field.setAccessible(true)
    field.set(null, true)
  }

  def map2java(map: Map[String, Any]): java.util.Map[String, Any] =
    map.view.mapValues(value2java).toMap.asJava

  def list2java(list: List[Any]): java.util.List[Any] =
    list.map(value2java).asJava

  private def value2java(value: Any): Any = value match {
    case value: Map[String, Any] => map2java(value)
    case value: List[Any] => list2java(value)
    case other => other
  }
}
