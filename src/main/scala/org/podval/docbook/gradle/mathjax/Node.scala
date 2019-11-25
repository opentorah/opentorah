package org.podval.docbook.gradle.mathjax

import java.io.File
import java.nio.file.{Files, Path, Paths}

import org.gradle.api.Project
import org.podval.docbook.gradle.util.{Architecture, Gradle, Logger, Os}

import scala.sys.process.{Process, ProcessBuilder, ProcessLogger}

final class Node(
  distribution: NodeDistribution,
  nodeRoot: File)
{
  override def toString: String = s"$distribution in $nodeRoot with modules in $nodeModules"

  private def isWindows: Boolean = distribution.isWindows

  val root: File = new File(nodeRoot, distribution.topDirectory)

  private val bin: File = if (distribution.hasBinSubdirectory) new File(root, "bin") else root

  private val nodeExec: File = new File(bin, if (isWindows) "node.exe" else "node")

  private def node(args: String*): String = {
    var out: Seq[String] = Seq.empty
    val addLine: String => Unit = (line: String) => out = out :+ line
    exec(
      command = nodeExec,
      args.toSeq,
      cwd = None,
      extraEnv = ("NODE_PATH", nodeModules.getAbsolutePath)
    ).!!(ProcessLogger(addLine, addLine))
    out.mkString("\n")
  }

  def evaluate(script: String): String = node("--print", script)

  private val npmExec: File = new File(bin, if (isWindows) "npm.cmd" else "npm")

  private def npm(args: String*): String = exec(
    command = npmExec,
    args.toSeq,
    cwd = Some(nodeRoot),
    extraEnv = ("PATH", npmExec.getParentFile.getAbsolutePath)
  ).!!

  val nodeModules: File = new File(nodeRoot, "node_modules")

  def npmInstall(module: String): String =
    npm("install", "--no-save", "--silent", module)

  private def exec(
    command: File,
    args: Seq[String],
    cwd: Option[File],
    extraEnv: (String, String)*
  ): ProcessBuilder = Process(
    command = command.getAbsolutePath +: args,
    cwd,
    extraEnv = extraEnv: _*
  )

  def postInstall(): Unit = {
    fixNpmSymlink()
  }

  private def fixNpmSymlink(): Unit = if (!isWindows) {
    val npm: Path = npmExec.toPath
    val deleted: Boolean = Files.deleteIfExists(npm)
    if (deleted) Files.createSymbolicLink(
      npm,
      bin.toPath.relativize(Paths.get(npmScriptFile.getAbsolutePath))
    )
  }

  private def npmScriptFile: File = {
    val lib: String = if (isWindows) "" else "lib/"
    new File(root, s"${lib}node_modules/npm/bin/npm-cli.js")
  }
}

object Node {

  val defaultVersion: String = "10.15.3"

  def install(project: Project, os: Os, arch: Architecture, into: File, logger: Logger): Node = {
    val version: String = defaultVersion

    val distribution: NodeDistribution = new NodeDistribution(version, os, arch)

    val installation: Node = new Node(distribution, into)

    if (!installation.root.exists()) {
      logger.info(s"Installing $installation")

      val nodeArtifact: File = Gradle.getArtifact(
        project,
        repositoryUrl = "https://nodejs.org/dist",
        artifactPattern = "v[revision]/[artifact](-v[revision]-[classifier]).[ext]",
        ivy = "v[revision]/ivy.xml",
        distribution.dependencyNotation
      )

      Gradle.unpack(
        project,
        archiveFile = nodeArtifact,
        isZip = distribution.isZip,
        into = into
      )

      installation.postInstall()
    }

    if (!installation.nodeModules.exists()) {
      logger.info(s"Installing mathjax-node")
      installation.npmInstall("mathjax-node")
    }

    installation
  }
}
