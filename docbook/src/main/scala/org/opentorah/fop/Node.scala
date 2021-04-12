package org.opentorah.fop

import org.opentorah.util.{Os, Platform}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.nio.file.{Files, Path, Paths}

final class Node(
  nodeModulesParent: File,
  val nodeExec: File,
  val npmExec: File
) {
  override def toString: String = s"Node with $nodeExec and $npmExec with modules in $nodeModules"

  def root: File = nodeModulesParent

  def exists: Boolean = nodeExec.exists && npmExec.exists

  val nodeModules: File = new File(nodeModulesParent, "node_modules")

  def fixup(distribution: NodeDistribution): Unit = if (!distribution.isWindows) {
    val npm: Path = npmExec.toPath
    val deleted: Boolean = Files.deleteIfExists(npm)
    if (deleted) {
      val npmCliJs: String = new File(root, s"lib/node_modules/npm/bin/npm-cli.js").getAbsolutePath
      Files.createSymbolicLink(
        npm,
        distribution.getBin(root).toPath.relativize(Paths.get(npmCliJs))
      )
    }
  }

  def evaluate(script: String): String = node(Seq("--print", script))

  private def node(args: Seq[String]): String = Platform.exec(
    command = nodeExec,
    args,
    cwd = None,
    extraEnv = ("NODE_PATH", nodeModules.getAbsolutePath)
  )

  def npmInstall(module: String, overwrite: Boolean): Unit = {
    if (overwrite || !nodeModules.exists) {
      Node.logger.info(s"Node.npmInstall($module)")
      nodeModules.mkdirs()
      npm(Seq("install", "--no-save", "--silent", module))
    }
  }

  private def npm(args: Seq[String]): String = Platform.exec(
    command = npmExec,
    args,
    // in local mode, npm puts packages into node_modules under the current working directory
    cwd = Some(nodeModulesParent),
    extraEnv = ("PATH", npmExec.getParentFile.getAbsolutePath)
  )
}

object Node {

  private val logger: Logger = LoggerFactory.getLogger(classOf[Node])

  def fromOs(nodeModulesParent: File): Option[Node] = {
    if (Platform.getOs == Os.Windows) None else {
      for {
        nodeExec <- Platform.which("node")
        npmExec <- Platform.which("npm")
      } yield new Node(
        nodeModulesParent,
        nodeExec,
        npmExec
      )
    }
  }

  def fromDistribution(distribution: NodeDistribution, into: File): Node = {
    val root: File = distribution.getRoot(into)
    val bin: File = distribution.getBin(root)

    new Node(
      nodeModulesParent = root,
      nodeExec = new File(bin, if (distribution.isWindows) "node.exe" else "node"),
      npmExec = new File(bin, if (distribution.isWindows) "npm.cmd" else "npm")
    )
  }
}
