package org.opentorah.build

import org.opentorah.util.Files
import java.io.File

trait InstallableDependency[T] extends Dependency:

  def repository: Option[Repository] = None

  // Where retrieved distributions are cached
  def cacheDirectory: String = artifact

  def archiveSubdirectoryPath(version: Version): Seq[String] = Seq.empty

  def isZip(version: Version): Boolean = false

  def installation(root: File): T

  def exists(installation: T): Boolean

  def fixup(installation: T): Unit = ()

  override def withVersion(version: Version): InstallableDependency.WithVersion[T] =
    InstallableDependency.WithVersion[T](dependency = this, version)

object InstallableDependency:
  final class WithVersion[T](
    override val dependency: InstallableDependency[T],
    version: Version
  ) extends Dependency.WithVersion(
    dependency,
    version
  ):
    def getInstallation(
      context: BuildContext,
      installIfDoesNotExist: Boolean,
      mustExist: Boolean
    ): T =
      val result: T = installation(context)

      if dependency.exists(result) then
        context.info(s"Existing $this detected: $result")
      else if installIfDoesNotExist then
        install(context)
        if !dependency.exists(result) then context.error(s"Failed to install dependency: $this")

      if !dependency.exists(result) && mustExist then context.fatalError(s"Needed dependency does not exist: $this")
      result

    private def install(context: BuildContext): Unit =
      val result: T = installation(context)
      context.lifecycle(s"Installing $this as $result")
      context.getArtifact(
        dependency.repository,
        dependencyNotation
      ).foreach((artifact: File) =>
        context.unpackArchive(
          file = artifact,
          isZip = dependency.isZip(version),
          installsInto(context)
        )
        dependency.fixup(result)
        require(dependency.exists(result), s"Does not exist after installation: $result")
      )

    private def installation(context: BuildContext): T =
      val into: File = installsInto(context)
      val root: File = Files.fileSeq(into, dependency.archiveSubdirectoryPath(version))
      dependency.installation(root)

    private def installsInto(context: BuildContext): File =
      Files.file(context.frameworks, dependency.cacheDirectory, fileName)
