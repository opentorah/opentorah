package org.podval.fop.mathjax

import java.io.File

import org.podval.fop.util.Logger

import scala.sys.process.{Process, ProcessBuilder, ProcessLogger}

class Node(
  nodeModulesParent: File,
  val nodeExec: File,
  val npmExec: File
) {
  override def toString: String = s"Node with $nodeExec and $npmExec with modules in $nodeModules"

  final def exists: Boolean = nodeExec.exists && npmExec.exists

  val nodeModules: File = new File(nodeModulesParent, "node_modules")

  final def evaluate(script: String): String = node(Seq("--print", script))

  private def node(args: Seq[String]): String = {
    var out: Seq[String] = Seq.empty
    val addLine: String => Unit = (line: String) => out = out :+ line
    exec(
      command = nodeExec,
      args,
      cwd = None,
      extraEnv = ("NODE_PATH", nodeModules.getAbsolutePath)
    ).!!(ProcessLogger(addLine, addLine))
    out.mkString("\n")
  }

  // Make sure MathJax is installed
  final def installMathJax(overwrite: Boolean, logger: Logger): Unit = {
    if (overwrite || !nodeModules.exists) {
      logger.info(s"Installing mathjax-node")
      npmInstall("mathjax-node")
    }
  }

  final def npmInstall(module: String): String =
    npm(Seq("install", "--no-save", "--silent", module))

  private def npm(args: Seq[String]): String = exec(
    command = npmExec,
    args,
    // in local mode, npm puts packages into node_modules under the current working directory
    cwd = Some(nodeModulesParent),
    extraEnv = ("PATH", npmExec.getParentFile.getAbsolutePath)
  ).!!

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
}
