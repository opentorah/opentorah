package org.podval.fop.mathjax

import java.io.File
import java.nio.file.{Files, Path, Paths}

final class NodeFromArtifact(
  nodeModulesParent: File,
  val root: File, // TODO expose via exists(); for FromOs - both paths...
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
    nodeRoot: File
  ): NodeFromArtifact = {
    val root: File = new File(nodeRoot, distribution.topDirectory)
    val bin: File = if (distribution.hasBinSubdirectory) new File(root, "bin") else root

    new NodeFromArtifact(
      nodeModulesParent = nodeRoot,
      root,
      bin,
      distribution.isWindows
    )
  }
}
