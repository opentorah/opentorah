package org.opentorah.build

import org.opentorah.util.{Files, Strings}
import java.io.File

abstract class InstallableDependency[T](
  group: String,
  artifact: String
) extends SimpleDependency(
  group,
  artifact
):
  override def apply(
    version: String
  ): InstallableDependency.WithVersion[T] = InstallableDependency.WithVersion[T](
    dependency = this,
    version = Dependency.Version(version)
  )

  final def installsInto(cacheRoot: File, version: Version): File =
    val artifactStr: String = artifactWithSuffix(version)
    val classifierStr: String = Strings.prefix("-", classifier(version))
    val extensionStr: String = extension(version).getOrElse("jar")
    val fileName: String = s"$artifactStr-${version.version}$classifierStr.$extensionStr"
    Files.file(cacheRoot, cacheDirectory, fileName)

  // Where retrieved distributions are cached
  def cacheDirectory: String = artifact

  def archiveSubdirectoryPath(version: Version): Seq[String] = Seq.empty

  def repository: Option[Repository] = None

  def isZip(version: Version): Boolean = false

  protected def installation(root: File): T

  protected def exists(installation: T): Boolean

  protected def fixup(installation: T): Unit = ()

object InstallableDependency:
  class WithVersion[T](
    override val dependency: InstallableDependency[T],
    version: dependency.Version
  ) extends Dependency.WithVersion(
    dependency,
    version
  ):
    def installsInto(context: BuildContext): File = dependency.installsInto(context.frameworks, version)

    def installation(context: BuildContext): T =
      val into: File = installsInto(context)
      val root: File = Files.fileSeq(into, dependency.archiveSubdirectoryPath(version))
      dependency.installation(root)

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

    def install(context: BuildContext): Unit =
      val result: T = installation(context)
      context.lifecycle(s"Installing $this as $result")
      context.getArtifact(
        dependency.repository,
        dependency.dependencyNotation(version)
      ).foreach((artifact: File) =>
        context.unpackArchive(
          file = artifact,
          isZip = dependency.isZip(version),
          installsInto(context)
        )
        dependency.fixup(result)
        require(dependency.exists(result), s"Does not exist after installation: $result")
      )

