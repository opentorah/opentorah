package org.opentorah.docbook.plugin

import org.gradle.api.Project
import org.gradle.api.tasks.SourceSet
import org.opentorah.docbook.Stylesheets
import org.opentorah.fop.{J2V8, J2V8Distribution, MathJaxRunner, Node, NodeDistribution}
import org.opentorah.mathjax.{MathJax, MathJaxConfiguration}
import org.opentorah.util.{Files, Gradle}
import java.io.File

object GradleOperations {

  private def info(project: Project, message: String): Unit = project.getLogger.info(message, null, null, null)

  def generateData(project: Project, mainClass: String, dataDirectory: File): Unit = {
    val mainSourceSet: Option[SourceSet] = Gradle.mainSourceSet(project)
    if (mainSourceSet.isEmpty) project.getLogger.warn(
      s"Skipping DocBook data generation: no Java plugin in the project")
    else {
      info(project, s"Running DocBook data generator $mainClass into $dataDirectory")
      Gradle.javaexec(
        project,
        mainClass,
        mainSourceSet.get.getRuntimeClasspath,
        dataDirectory.toString
      )
    }
  }

  def unpackStylesheets(project: Project, stylesheets: Stylesheets, version: String, root: File): File = {
    val file: File = Gradle.getArtifact(project, stylesheets.dependencyNotation(version)).get
    val into: File = new File(root, file.getName)
    val directory: File = Files.file(into, stylesheets.archiveSubdirectoryPath)
    if (!directory.exists) Gradle.unpack(
      project,
      file,
      isZip = true,
      into
    )

    directory
  }

  def getMathJaxRunner(
    project: Project,
    nodeRoot: File,
    nodeVersion: String,
    overwriteNode: Boolean,
    overwriteMathJax: Boolean,
    j2v8Parent: Option[File],
    mathJax: MathJax,
    configuration: MathJaxConfiguration
  ): MathJaxRunner = MathJaxRunner.get(
    node = installNode(
      project,
      new NodeDistribution(nodeVersion),
      into = nodeRoot,
      overwrite = overwriteNode
    ),
    overwriteMathJax,
    j2v8 = j2v8Parent.flatMap(j2v8Parent => installJ2V8(
      project,
      new J2V8Distribution,
      into = j2v8Parent
    )),
    mathJax,
    configuration
  )

  private def installNode(
    project: Project,
    nodeDistribution: NodeDistribution,
    into: File,
    overwrite: Boolean
  ): Node = {
    val result: Node = Node.fromDistribution(nodeDistribution, into)

    if (!overwrite && result.exists) info(project, s"Existing installation detected: $result") else {
      info(project, s"Installing $this as $result")

      val artifact: File = Gradle.getArtifact(
        project,
        repositoryUrl = "https://nodejs.org/dist",
        artifactPattern = "v[revision]/[artifact](-v[revision]-[classifier]).[ext]",
        ivy = "v[revision]/ivy.xml",
        nodeDistribution.dependencyNotation
      )

      Gradle.unpack(
        project,
        archiveFile = artifact,
        nodeDistribution.isZip,
        into
      )

      result.fixup(nodeDistribution)
    }

    result
  }

  private def installJ2V8(project: Project, j2v8Dictribution: J2V8Distribution, into: File): Option[J2V8] =
    if (j2v8Dictribution.version.isEmpty) {
      project.getLogger.warn(s"No $j2v8Dictribution")
      None
    } else Gradle.getArtifact(project, j2v8Dictribution.dependencyNotation).map { artifact =>
      Gradle.unpack(
        project,
        artifact,
        isZip = true,
        into
      )
      new J2V8(libraryPath = new File(into, j2v8Dictribution.libraryName).getAbsolutePath)
    }
}
