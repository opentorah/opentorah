package org.opentorah.fop.mathjax

import java.io.File
import java.nio.file.{Files, Path, Paths}

// TODO stick NodeDistribution into Node (again?); make names of the executables methods on it...
final class NodeFromArtifact(
  nodeModulesParent: File,
  root: File,
  bin: File,
  isWindows: Boolean
) extends Node(
  nodeModulesParent,
  nodeExec = new File(bin, if (isWindows) "node.exe" else "node"),
  npmExec = new File(bin, if (isWindows) "npm.cmd" else "npm")
) {
  override def toString: String = s"Node in $root with $nodeExec and $npmExec with modules in $nodeModules"

  def postInstall(): Unit = {
    fixNpmSymlink()
  }

  private def fixNpmSymlink(): Unit = if (!isWindows) {
    val npm: Path = npmExec.toPath
    val deleted: Boolean = Files.deleteIfExists(npm)
    if (deleted) {
      val npmCliJs: String = new File(root, s"lib/node_modules/npm/bin/npm-cli.js").getAbsolutePath
      Files.createSymbolicLink(
        npm,
        bin.toPath.relativize(Paths.get(npmCliJs))
      )
    }
  }
}

object NodeFromArtifact {

  def apply(
    distribution: NodeDistribution,
    nodeRoot: File,
    nodeModulesParent: File
  ): NodeFromArtifact = {
    val root: File = new File(nodeRoot, distribution.topDirectory)
    val bin: File = if (distribution.hasBinSubdirectory) new File(root, "bin") else root

    new NodeFromArtifact(
      nodeModulesParent = nodeModulesParent,
      root,
      bin,
      distribution.isWindows
    )
  }
}
