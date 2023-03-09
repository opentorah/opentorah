package org.opentorah.node

import org.opentorah.platform.Exec
import org.slf4j.{Logger, LoggerFactory}
import java.io.File

final class Node(
  val installation: NodeInstallation,
  val nodeModulesParent: File
):
  override def toString: String = s"Node with modules in $nodeModules and $installation"

  val nodeModules: File = File(nodeModulesParent, "node_modules")

  def evaluate(script: String, useEsm: Boolean): String =
    node((if useEsm then "--require esm" else "") + "--no-warnings --print " + script)

  val nodeEnv: (String, String) = ("NODE_PATH", nodeModules.getAbsolutePath)

  def node(args: String): String = Exec(
    command = installation.nodeExec,
    args,
    cwd = None,
    extraEnv = nodeEnv
  )

  def npmInitPrivate(): Unit = npm("init private")
  
  def npmInstall(modules: Seq[String]): Unit =
    val modulesStr = modules.mkString(", ")
    Node.logger.info(s"Node.npmInstall($modulesStr)")
    nodeModules.mkdirs()
    npm("install " + modules.mkString(" "))

  // npm init esm --yes

  def npm(args: String): String = Exec(
    command = installation.npmExec,
    args,
    // in local mode, npm puts packages into node_modules under the current working directory
    cwd = Some(nodeModulesParent),
    // TODO do I need the system path here?
    extraEnv = nodeEnv, ("PATH", installation.getBin.getAbsolutePath + ":" + System.getenv("PATH"))
  )

object Node:
  private val logger: Logger = LoggerFactory.getLogger(classOf[Node])
