package org.opentorah.node

import java.io.File
import org.opentorah.util.{Os, Platform}
import org.slf4j.{Logger, LoggerFactory}

class Node(
  nodeModulesParent: File,
  val nodeExec: File,
  val npmExec: File
) {
  final override def toString: String = s"Node with $nodeExec and $npmExec with modules in $nodeModules"

  final def exists: Boolean = nodeExec.exists && npmExec.exists

  val nodeModules: File = new File(nodeModulesParent, "node_modules")

  final def evaluate(script: String): String = node(Seq("--print", script))

  private def node(args: Seq[String]): String = Platform.exec(
    command = nodeExec,
    args,
    cwd = None,
    extraEnv = ("NODE_PATH", nodeModules.getAbsolutePath)
  )

  final def npmInstall(module: String, overwrite: Boolean): Unit = {
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
}
