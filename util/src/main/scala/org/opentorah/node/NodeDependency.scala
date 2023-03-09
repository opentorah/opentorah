package org.opentorah.node

import org.opentorah.build.{Dependency, InstallableDependency, Repository}
import org.opentorah.platform.{Architecture, Os}
import org.opentorah.util.Strings
import java.io.File
import java.nio.file.{Files, Path, Paths}

// Heavily inspired by (read: copied and reworked from :)) https://github.com/srs/gradle-node-plugin by srs.
// That plugin is not used directly because its tasks are not reusable unless the plugin is applied to the project,
// and I do not want to apply Node plugin to every project that uses DocBook, for instance.
// Also, I want to be able to run npm from within my code without creating tasks.
// Also, I would like to be able to use Node available via GraalVM's polyglot support.
// My simplified Node support is under 200 lines.

// Describes Node distribution's packaging and structure.
object NodeDependency extends InstallableDependency[NodeInstallation](
  group = "org.nodejs",
  artifact = "node"
):
  // Anything later than that breaks ScalaJS: 17.9.1, 18.15.0, 19.8.1
  val versionDefault: String = "16.19.1"

  override def cacheDirectory: String = "nodejs"

  override def repository: Option[Repository] = Some(Repository(
    url = "https://nodejs.org/dist",
    artifactPattern = "v[revision]/[artifact](-v[revision]-[classifier]).[ext]",
    ivy = "v[revision]/ivy.xml"
  ))

  private val os: Os = Os.get
  private val isWindows: Boolean = os == Os.Windows

  private val architecture: Architecture = Architecture.get

// TODO override def toString: String = s"Node v$version for $os on $architecture"

  private val osName: String = os match
    case Os.Windows => "win"
    case Os.Mac     => "darwin"
    case Os.Linux   => "linux"
    case Os.FreeBSD => "linux"
    case Os.SunOS   => "sunos"
    case Os.Aix     => "aix"
    case _ => throw IllegalArgumentException(s"Unsupported OS: $os")

  private val osArch: String = architecture match
    case Architecture.x86_64  => "x64"
    case Architecture.amd64   => "x64"
    case Architecture.aarch64 => "x64"
    case Architecture.ppc64   => "ppc64"
    case Architecture.ppc64le => "ppc64le"
    case Architecture.s390x   => "s390x"
    case Architecture.armv6l  => "armv6l"
    case Architecture.armv7l  => "armv7l"
    case Architecture.armv8l  => "arm64" // *not* "armv8l"!
    case Architecture.i686    => "x86"
    case Architecture.nacl    => "x86"

  //https://github.com/nodejs/node/pull/5995
  private def hasWindowsZip(version: Dependency.Version): Boolean =
    // TODO move into DependencyVersion
    val versionTokens: Array[String] = version.version.split('.')
    val majorVersion: Int = versionTokens(0).toInt
    val minorVersion: Int = versionTokens(1).toInt
    val microVersion: Int = versionTokens(2).toInt

    ((majorVersion == 4) && (minorVersion >= 5)) || // >= 4.5.0..6
    ((majorVersion == 6) && ((minorVersion > 2) || ((minorVersion == 2) && (microVersion >= 1)))) || // >= 6.2.1..7
     (majorVersion > 6) // 7..

  override def classifier(version: Dependency.Version): Option[String] =
    val fixUpOsAndArch: Boolean = isWindows && !hasWindowsZip(version)
    val dependencyOsName: String = if fixUpOsAndArch then "linux" else osName
    val dependencyOsArch: String = if fixUpOsAndArch then "x86"   else osArch
    Some(s"$dependencyOsName-$dependencyOsArch")

  override def isZip(version: Dependency.Version): Boolean = isWindows && hasWindowsZip(version)

  override def extension(version: Dependency.Version): Option[String] = Some(if isZip(version) then "zip" else "tar.gz")

  override def archiveSubdirectoryPath(version: Dependency.Version): Seq[String] =
    val classifierStr: String = Strings.prefix("-", classifier(version))
    Seq(
      s"$artifact-v${version.version}$classifierStr"
    )

  override def installation(root: File): NodeInstallation =
    val bin: File = if !isWindows then File(root, "bin") else root

    NodeInstallation(
      nodeExec = File(bin, if isWindows then "node.exe" else "node"),
      npmExec  = File(bin, if isWindows then "npm.cmd"  else "npm" )
    )

  override protected def exists(installation: NodeInstallation): Boolean =
    installation.nodeExec.exists && installation.npmExec.exists

  override protected def fixup(installation: NodeInstallation): Unit = if !isWindows then
    val npm: Path = installation.npmExec.toPath
    val deleted: Boolean = Files.deleteIfExists(npm)
    if deleted then
      val root: File = installation.getRoot
      val npmCliJs: String = File(root, s"lib/node_modules/npm/bin/npm-cli.js").getAbsolutePath
      Files.createSymbolicLink(
        npm,
        npm.getParent.relativize(Paths.get(npmCliJs))
      )
