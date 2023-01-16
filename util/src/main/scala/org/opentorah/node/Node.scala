package org.opentorah.node

import org.opentorah.platform.{Exec, Os}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.nio.file.{Files, Path, Paths}

final class Node(
  nodeModulesParent: File,
  nodeExec: File,
  npmExec: File
):
  override def toString: String = s"Node with $nodeExec\n and $npmExec\n with modules in $nodeModules"

  def exists: Boolean = nodeExec.exists && npmExec.exists
  
  def root: File = nodeModulesParent

  private val nodeModules: File = File(nodeModulesParent, "node_modules")

  def fixup(distribution: NodeDistribution): Unit = if !distribution.isWindows then
    val npm: Path = npmExec.toPath
    val deleted: Boolean = Files.deleteIfExists(npm)
    if deleted then
      val npmCliJs: String = File(root, s"lib/node_modules/npm/bin/npm-cli.js").getAbsolutePath
      Files.createSymbolicLink(
        npm,
        distribution.getBin(root).toPath.relativize(Paths.get(npmCliJs))
      )

  def evaluate(script: String, useEsm: Boolean): String = node(
    (if useEsm then Seq("--require", "esm") else Seq.empty) ++
    Seq("--no-warnings", "--print", script)
  )

  private def node(args: Seq[String]): String = Exec(
    command = nodeExec,
    args,
    cwd = None,
    extraEnv = ("NODE_PATH", nodeModules.getAbsolutePath)
  )

  private var installed: Set[String] = Set.empty

  def npmInstall(module: String): Unit = if !installed.contains(module) then
    Node.logger.info(s"Node.npmInstall($module)")
    nodeModules.mkdirs()
    npm(Seq("install", "--no-save", "--silent", module))
    installed = installed + module

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
