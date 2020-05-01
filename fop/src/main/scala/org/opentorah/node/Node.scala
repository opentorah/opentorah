package org.opentorah.node

import java.io.File
import org.gradle.api.Project
import org.opentorah.util.{Gradle, Os, Platform}
import org.slf4j.{Logger, LoggerFactory}
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
    exec(
      command = nodeExec,
      args,
      cwd = None,
      extraEnv = ("NODE_PATH", nodeModules.getAbsolutePath)
    ).!(ProcessLogger(line => Node.logger.debug(line), line => out = out :+ line))
    out.mkString("\n")
  }

  final def install(name: String, overwrite: Boolean): Unit = {
    if (overwrite || !nodeModules.exists) {
      Node.logger.info(s"Installing '$name'")
      nodeModules.mkdirs()
      npmInstall(name)
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
  ): ProcessBuilder = {
    val cmd: Seq[String] = command.getAbsolutePath +: args
    Node.logger.debug(
      s"""Node.exec(
         |  cmd = $cmd,
         |  cwd = $cwd,
         |  extraEnv = $extraEnv
         |)""".stripMargin
    )

    Process(
      command = cmd,
      cwd,
      extraEnv = extraEnv: _*
    )
  }
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

  def install(project: Project, into: File, overwrite: Boolean, nodeModulesParent: File): Node = {
    val distribution: NodeDistribution = new NodeDistribution
    val node: NodeFromArtifact = NodeFromArtifact(distribution, into, nodeModulesParent)

    if (!overwrite && node.exists) {
      logger.info(s"Existing installation detected: $node")
    } else {
      logger.info(s"Installing $node")

      val artifact: File = Gradle.getArtifact(
        project,
        repositoryUrl = "https://nodejs.org/dist",
        artifactPattern = "v[revision]/[artifact](-v[revision]-[classifier]).[ext]",
        ivy = "v[revision]/ivy.xml",
        dependencyNotation = distribution.dependencyNotation
      )

      logger.info(s"Resolved $distribution: $artifact")

      into.mkdirs()

      Gradle.unpack(
        project,
        archiveFile = artifact,
        isZip = distribution.isZip,
        into = into
      )

      logger.info(s"Extracted $distribution into $into")

      node.postInstall()
    }

    node
  }
}
