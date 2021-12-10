package org.opentorah.util

import java.io.File

abstract class Distribution[T](version: String):
  override def toString: String

  // Where retrieved distributions are cached
  protected def cacheDirectory: String
  protected def repository: Option[Repository] = None
  protected def groupId: String
  protected def artifactId: String
  protected def classifier: Option[String]
  protected def extension: Option[String]
  protected def isZip: Boolean
  protected def archiveSubdirectoryPath: Seq[String]
  protected def installation(root: File): T
  protected def exists(installation: T): Boolean
  protected def fixup(installation: T): Unit = {}

  final protected def dependencyNotation: String =
    s"$groupId:$artifactId:$version${Distribution.prefix(":", classifier)}${Distribution.prefix("@", extension)}"

  private def fileName: String =
    s"$artifactId-$version${Distribution.prefix("-", classifier)}${Distribution.prefix(".", extension)}"

  final def getInstallation(context: BuildContext): Option[T] =
    val into: File = Files.file(context.frameworks, cacheDirectory, fileName)
    val root: File = Files.fileSeq(into, archiveSubdirectoryPath)
    val result: T = installation(root)

    if exists(result) then
      context.info(s"Existing $this detected: $result")
      Some(result)
    else
      context.info(s"Installing $this as $result")
      context.getArtifact(
        repository,
        dependencyNotation
      ).map(artifact =>
        context.unpackArchive(
          file = artifact,
          isZip = isZip,
          into
        )
        fixup(result)
        require(exists(result))
        result
      )

object Distribution:
  // TODO move into Strings
  def prefix(prefix: String, what: Option[String]): String = what.fold("")(string => prefix + string)

