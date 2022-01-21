package org.opentorah.build

import org.gradle.api.Project
import org.opentorah.util.Logging
import org.slf4j.Logger
import java.io.File

// Integration with the build tool or other execution context.
trait BuildContext:
  // Frameworks
  // Although Gradle caches resolved artifacts and npm caches packages that it retrieves,
  // unpacking frameworks under `/build` after each `./gradlew clean` takes noticeable time (around 14 seconds);
  // so, I am caching unpacked frameworks under `~/.gradle`.
  def frameworks: File

  // Logging
  def getLogger: Logger
  final def warn(message: String): Unit = getLogger.warn(message)
  def lifecycle(message: String): Unit
  final def info(message: String): Unit = getLogger.info(message)

  def getArtifact(repository: Option[Repository], dependencyNotation: String): Option[File]

  def unpackArchive(file: File, isZip: Boolean, into: File): Unit

  def javaexec(mainClass: String, args: String*): Unit

object BuildContext:

  def forGradleProject(project: Project): BuildContext = GradleBuildContext(project)

  def default(logger: Logger): BuildContext =
    val home: Option[String] = Option(System.getenv("HOME"))
    val frameworksDir: File = File(home.map(_ + "/.gradle").getOrElse("/tmp"))
    Logging.setWarn(logger)
    Default(logger, frameworksDir)

  final class Default(logger: Logger, override val frameworks: File) extends BuildContext:
    override def getLogger: Logger = logger
    override def lifecycle(message: String): Unit = logger.warn(message)

    override def getArtifact(repository: Option[Repository], dependencyNotation: String): Option[File] = None
    override def unpackArchive(file: File, isZip: Boolean, into: File): Unit =
      throw UnsupportedOperationException("unpackArchive() is not available")

    override def javaexec(mainClass: String, args: String*): Unit = Class
      .forName(mainClass)
      .getMethod("main", classOf[Array[String]])
      .invoke(null, args.toArray)
