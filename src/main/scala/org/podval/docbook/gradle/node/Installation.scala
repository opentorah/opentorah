package org.podval.docbook.gradle.node

import java.io.File
import java.nio.file.{Files, Path, Paths}

import org.gradle.api.Project
import org.podval.docbook.gradle.Logger
import org.podval.docbook.gradle.util.{Architecture, Gradle, Os, Platform}

import scala.sys.process.Process

final class Installation(
  distribution: Distribution,
  nodeRoot: File,
  nodeModulesRoot: File)
{
  private def isWindows: Boolean = distribution.isWindows

  val nodeDir: File =
    new File(nodeRoot, distribution.topDirectory)

  private val nodeBinDir: File =
    if (distribution.hasBinSubdirectory) new File(nodeDir, "bin") else nodeDir

  private val npmScriptFile: File = {
    val lib: String = if (isWindows) "" else "lib/"
    new File(nodeDir, s"${lib}node_modules/npm/bin/npm-cli.js")
  }

  private val npmExec: File =
    new File(nodeBinDir, if (isWindows) "npm.cmd" else "npm")

//  private val nodeExec: File =
//    new File(nodeBinDir, if (isWindows) "node.exe" else "node")

  def isInstalled: Boolean = nodeDir.exists

  def install(project: Project): Unit = {
    val nodeArchive: File = Gradle.getArtifact(project, distribution.dependencyNotation, Distribution.repository)

    Gradle.unpack(project, nodeArchive, distribution.isZip, nodeRoot)

    fixNpmSymlink()
  }

  private def fixNpmSymlink(): Unit = if (!isWindows) {
    val npm: Path = npmExec.toPath
    val deleted: Boolean = Files.deleteIfExists(npm)
    if (deleted) Files.createSymbolicLink(
      npm,
      nodeBinDir.toPath.relativize(Paths.get(npmScriptFile.getAbsolutePath))
    )
  }

  private def npm(args: Seq[String], cwd: File): String = Process(
    command = npmExec.getAbsolutePath +: args,
    cwd = Some(cwd),
    ("PATH", npmExec.getParentFile.getAbsolutePath)
  ).!!

  def npmInstall(module: String): String =
    npm(Seq("install", "--no-save", "--silent", module), cwd = nodeModulesRoot)

  def isNodeModulesInstalled: Boolean =
    new File(nodeModulesRoot, "node_modules").exists
}

object Installation {

  def installMathJax(
    nodeRoot: File,
    nodeModulesRoot: File,
    project: Project,
    logger: Logger
  ): Installation = {
    val version: String = Distribution.defaultVersion
    val os: Os = Platform.getOs
    val arch: Architecture = Platform.getArch

    val distribution: Distribution = new Distribution(version, os, arch)
    val installation: Installation = new Installation(distribution, nodeRoot, nodeModulesRoot)

    if (!installation.isInstalled) {
      logger.info(s"Installing Node v$version for $os on $arch")
      installation.install(project)
    }

    if (!installation.isNodeModulesInstalled) {
      logger.info(s"Installing mathjax-node")
      installation.npmInstall("mathjax-node")
    }

    installation
  }
}
