package org.opentorah.fop

import org.opentorah.util.Platform
import org.slf4j.{Logger, LoggerFactory}
import java.io.File

// Heavily inspired by (read: copied and reworked from :)) https://github.com/srs/gradle-node-plugin by srs.
// That plugin is not used directly because its tasks are not reusable unless the plugin is applied to the project,
// and I do not want to apply Node plugin to every project that uses DocBook.
// Also, I want to be able to run npm from within my code without creating tasks.
// My simplified Node support is under 200 lines.

// Describes Node distribution's packaging and structure.
final class NodeDistribution(val version: String):
  private val logger: Logger = LoggerFactory.getLogger(classOf[NodeDistribution])

  val os: Platform.Os = Platform.getOs
  val architecture: Platform.Architecture = Platform.getArch

  override def toString: String = s"Node v$version for $os on $architecture"

  private val osName: String = os match
    case Platform.Os.Windows => "win"
    case Platform.Os.Mac     => "darwin"
    case Platform.Os.Linux   => "linux"
    case Platform.Os.FreeBSD => "linux"
    case Platform.Os.SunOS   => "sunos"
    case Platform.Os.Aix     => "aix"
    case _          => throw IllegalArgumentException (s"Unsupported OS: $os")

  val isWindows: Boolean = os == Platform.Os.Windows

  private val osArch: String = architecture match
    case Platform.Architecture.x86_64  => "x64"
    case Platform.Architecture.amd64   => "x64"
    case Platform.Architecture.aarch64 => "x64"
    case Platform.Architecture.ppc64   => "ppc64"
    case Platform.Architecture.ppc64le => "ppc64le"
    case Platform.Architecture.s390x   => "s390x"
    case Platform.Architecture.armv6l  => "armv6l"
    case Platform.Architecture.armv7l  => "armv7l"
    case Platform.Architecture.armv8l  => "arm64" // *not* "armv8l"!
    case Platform.Architecture.i686    => "x86"
    case Platform.Architecture.nacl    => "x86"

  private val versionTokens: Array[String] = version.split('.')
  private val majorVersion: Int = versionTokens(0).toInt
  private val minorVersion: Int = versionTokens(1).toInt
  private val microVersion: Int = versionTokens(2).toInt

  //https://github.com/nodejs/node/pull/5995
  private def hasWindowsZip: Boolean =
    ((majorVersion == 4) && (minorVersion >= 5)) || // >= 4.5.0..6
    ((majorVersion == 6) && ((minorVersion > 2) || ((minorVersion == 2) && (microVersion >= 1)))) || // >= 6.2.1..7
     (majorVersion >  6) // 7..

  private def fixUpOsAndArch: Boolean = isWindows && !hasWindowsZip
  private val dependencyOsName: String = if fixUpOsAndArch then "linux" else osName
  private val dependencyOsArch: String = if fixUpOsAndArch then "x86" else osArch

  def isZip: Boolean = isWindows && hasWindowsZip
  private val ext: String = if isZip then "zip" else "tar.gz"

  val dependencyNotation: String =
    s"org.nodejs:node:$version:$dependencyOsName-$dependencyOsArch@$ext"

  def getRoot(into: File): File = File(into, topDirectory)

  val topDirectory: String =
    s"node-v$version-$dependencyOsName-$dependencyOsArch"

  def getBin(root: File): File = if hasBinSubdirectory then File(root, "bin") else root

  def hasBinSubdirectory: Boolean = !isWindows
