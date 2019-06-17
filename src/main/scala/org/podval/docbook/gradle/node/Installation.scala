package org.podval.docbook.gradle.node

import java.io.File
import java.nio.file.{Files, Path, Paths}

import scala.sys.process.Process

final class Installation(
  val distribution: Distribution,
  val nodeRoot: File,
  val nodeModulesRoot: File)
{
  override def toString: String = s"$distribution in $nodeRoot with modules in $nodeModules"

  private def isWindows: Boolean = distribution.isWindows

  val root: File = new File(nodeRoot, distribution.topDirectory)

  private val bin: File = if (distribution.hasBinSubdirectory) new File(root, "bin") else root

  private val nodeExec: File = new File(bin, if (isWindows) "node.exe" else "node")

  /////  def node(args)
  /////  def evaluate(script: String): String = ??? // node -p <script>

  private val npmExec: File = new File(bin, if (isWindows) "npm.cmd" else "npm")

  private def npmScriptFile: File = {
    val lib: String = if (isWindows) "" else "lib/"
    new File(root, s"${lib}node_modules/npm/bin/npm-cli.js")
  }

  private def npm(args: Seq[String], cwd: File): String = Process(
    command = npmExec.getAbsolutePath +: args,
    cwd = Some(cwd),
    ("PATH", npmExec.getParentFile.getAbsolutePath)
  ).!!

  val nodeModules: File = new File(nodeModulesRoot, "node_modules")

  def npmInstall(module: String): String =
    npm(Seq("install", "--no-save", "--silent", module), cwd = nodeModulesRoot)

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
}
