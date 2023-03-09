package org.opentorah.node

import org.opentorah.platform.{Exec, Os}
import java.io.File

final class NodeInstallation(
  val nodeExec: File,
  val npmExec: File
):
  override def toString: String = s"Node installation with root $getRoot"

  // TODO get the version of node

  // Note: if installation was not installed from the distribution, root is meaningless
  def getRoot: File =
    val result: File = nodeExec.getParentFile
    if result.getName == "bin" then result.getParentFile else result

  def getBin: File = nodeExec.getParentFile

  def getNode(nodeModulesParent: File): Node = Node(this, nodeModulesParent)

object NodeInstallation:

  def fromOs: Option[NodeInstallation] = if Os.get == Os.Windows then None else
    for
      nodeExec <- Exec.which("node")
      npmExec  <- Exec.which("npm")
    yield NodeInstallation(
      nodeExec,
      npmExec
    )
