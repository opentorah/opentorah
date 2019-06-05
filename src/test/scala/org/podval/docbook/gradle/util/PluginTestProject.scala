package org.podval.docbook.gradle.util

import java.io.File

import org.gradle.testkit.runner.GradleRunner
import org.podval.docbook.gradle.plugin.{DocBook, Layout, Write}
import org.podval.docbook.gradle.xml.Xml

class PluginTestProject(projectDir: File) {
  projectDir.mkdirs()

  private val write: Write = new Write(
    layout = Layout.forRoot(projectDir),
    logger = new TestLogger)

  def layout: Layout = write.layout

  def destroy(): Unit = Util.deleteRecursively(projectDir)

  def run(): String = getRunner.build.getOutput

  def fail(): String = getRunner.buildAndFail.getOutput

  private def getRunner: GradleRunner = GradleRunner.create
    .withProjectDir(projectDir)
    .withArguments("-i", "processDocBook")
}

object PluginTestProject {

  def apply(
    name: String,
    prefix: Option[String] = None,
    document: String = s"<article ${DocBook.Namespace.withVersion}/>",
    substitutions: Map[String, String] = Map.empty
  ): PluginTestProject = {
    val layout: Layout = Layout.forCurrent
    val documentName: String = "test"
    val projectDir: File = new File(Util.prefixedDirectory(layout.buildDir, prefix), name)
    val result: PluginTestProject = new PluginTestProject(projectDir)
    result.write.settingsGradle(layout.projectDir)
    result.write.buildGradle(
        documentName = documentName,
        document = document,
        substitutions = substitutions
      )
    result.write.inputFile(documentName, s"${Xml.header}\n$document")
    result
  }
}
