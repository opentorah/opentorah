package org.opentorah.math

import org.opentorah.platform.{Exec, Os}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.nio.file.{Files, Path, Paths}

final class Node(
  nodeModulesParent: File,
  val nodeExec: File,
  val npmExec: File
):
  override def toString: String = s"Node with $nodeExec and $npmExec with modules in $nodeModules"

  def root: File = nodeModulesParent

  val nodeModules: File = File(nodeModulesParent, "node_modules")

  def fixup(distribution: NodeDistribution): Unit = if !distribution.isWindows then
    val npm: Path = npmExec.toPath
    val deleted: Boolean = Files.deleteIfExists(npm)
    if deleted then
      val npmCliJs: String = File(root, s"lib/node_modules/npm/bin/npm-cli.js").getAbsolutePath
      Files.createSymbolicLink(
        npm,
        distribution.getBin(root).toPath.relativize(Paths.get(npmCliJs))
      )

  def evaluate(script: String): String = node(Seq("--print", script))

  private def node(args: Seq[String]): String = Exec(
    command = nodeExec,
    args,
    cwd = None,
    extraEnv = ("NODE_PATH", nodeModules.getAbsolutePath)
  )

  def npmInstall(module: String, overwrite: Boolean): Unit =
    if overwrite || !nodeModules.exists then
      Node.logger.info(s"Node.npmInstall($module)")
      nodeModules.mkdirs()
      npm(Seq("install", "--no-save", "--silent", module))

  private def npm(args: Seq[String]): String = Exec(
    command = npmExec,
    args,
    // in local mode, npm puts packages into node_modules under the current working directory
    cwd = Some(nodeModulesParent),
    extraEnv = ("PATH", npmExec.getParentFile.getAbsolutePath)
  )

object Node:

  // TODO unify with ProcessContext's logger
  private val logger: Logger = LoggerFactory.getLogger(classOf[Node])

  def fromOs(nodeModulesParent: File): Option[Node] =
    if Os.get == Os.Windows then None else
      for
        nodeExec <- Exec.which("node")
        npmExec  <- Exec.which("npm")
      yield Node(
        nodeModulesParent,
        nodeExec,
        npmExec
      )
