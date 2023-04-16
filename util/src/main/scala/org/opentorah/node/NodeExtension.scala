package org.opentorah.node

import org.gradle.api.{DefaultTask, Project}
import org.gradle.api.logging.LogLevel
import org.gradle.api.provider.{ListProperty, Property}
import org.gradle.api.tasks.{Input, TaskAction}
import org.opentorah.build.Gradle.*
import org.opentorah.build.{Gradle, GradleBuildContext}
import org.opentorah.node.{Node, NodeDependency, NodeInstallation}
import javax.inject.Inject
import java.io.File

abstract class NodeExtension @Inject(project: Project):
  def getVersion: Property[String]
  def version: Option[String] = getVersion.toOption

  def getModules: ListProperty[String]
  def modules: List[String] = getModules.toList

  def node: Node = node(installIfDoesNotExist = false)
  def node(installIfDoesNotExist: Boolean): Node =
    val installation: NodeInstallation = version match
      case None =>
        NodeInstallation.fromOs.get
      case Some(version) =>
        NodeDependency(version).getInstallation(
          GradleBuildContext(project),
          installIfDoesNotExist = installIfDoesNotExist,
          mustExist = true
        )

    installation.getNode(nodeModulesParent = project.getProjectDir)

  def node(arguments: String): Unit = node(arguments, LogLevel.LIFECYCLE)
  def node(arguments: String, logLevel: LogLevel): Unit = node.node(arguments, Gradle.log(project, logLevel))

  def npm(arguments: String): Unit = npm(arguments, LogLevel.LIFECYCLE)
  def npm(arguments: String, logLevel: LogLevel): Unit = node.npm(arguments, Gradle.log(project, logLevel))

  def setUpProject(requiredModules: List[String]): Unit =
    val isProjectSetUp: Boolean = File(project.getProjectDir, "package.json").exists

    // Initialize Node project
    if !isProjectSetUp then npm(arguments = "init private")

    // Install Node modules
    node.mkNodeModules()
    npm(
      arguments = "install " + (requiredModules ++ modules).mkString(" "),
      logLevel = if isProjectSetUp then LogLevel.INFO else LogLevel.LIFECYCLE
    )

object NodeExtension:
  def addTo(project: Project): Unit =
    project.getExtensions.create("node", classOf[NodeExtension])
    project.getTasks.create("npm" , classOf[NpmTask])
    project.getTasks.create("node", classOf[NodeTask])

  def get(project: Project): NodeExtension = project.getExtensions.getByType(classOf[NodeExtension])

  class NodeTask extends DefaultTask:
    setGroup("other")
    setDescription("Run commands with 'node'")

    private var arguments: String = ""
    @TaskAction def execute(): Unit = get(getProject).node(arguments)
    @Input def getArguments: String = arguments

    @org.gradle.api.tasks.options.Option(option = "node-arguments", description = "The command to execute with 'node'")
    def setArguments(value: String): Unit = arguments = value

  class NpmTask extends DefaultTask:
    setGroup("other")
    setDescription("Run commands with 'npm'")

    private var arguments: String = ""
    @TaskAction def execute(): Unit = get(getProject).npm(arguments)
    @Input def getArguments: String = arguments

    @org.gradle.api.tasks.options.Option(option = "npm-arguments", description = "The command to execute with 'npm'")
    def setArguments(value: String): Unit = arguments = value
