package org.podval.docbook.gradle

import org.gradle.testkit.runner.GradleRunner
import java.io.File

import org.podval.docbook.gradle.util.Util
import org.podval.docbook.gradle.xml.Xml

class PluginTestProject(projectDir: File) {
  projectDir.mkdirs()

  val layout: Layout = Layout.forRoot(projectDir)

  private val logger: Logger = new TestLogger

  private def writeInto(fileName: String)(content: String): Unit =
    Util.writeInto(new File(projectDir, fileName), logger)(content)

  def writeSettingsGradle(pluginDir: File): Unit =
    writeInto("settings.gradle")(s"includeBuild '$pluginDir'")

  private val buildGradleCommon: String =
    s"""plugins {
       |  id 'org.podval.docbook-gradle-plugin' version '1.0.0'
       |  id 'base'
       |}
       |
       |repositories {
       |  jcenter()
       |}
       |"""

  def writeBuildGradle(): Unit =     writeInto("build.gradle")(buildGradleCommon)

  def writeBuildGradleWithDocument(
    documentName: String,
    document: String,
    substitutions: Map[String, String] = Map.empty
  ): Unit = {
    writeInto("build.gradle") {
      val substitutionsFormatted: String = if (substitutions.isEmpty) "" else {
        val contents: String = substitutions.map { case (name: String, value: String) =>
          s""""$name": $value"""
        }.mkString(",\n")

        s"""
           |  substitutions = [
           |    $contents
           |  ]
           |"""
      }

      s"""$buildGradleCommon
         |
         |docBook {
         |  document = "$documentName"
         |  outputFormats = ["html"]
         |$substitutionsFormatted
         |}
         |"""
    }

    Util.writeInto(layout.inputFile(documentName), logger)(s"${Xml.header}\n$document")
  }

  def destroy(): Unit = Util.deleteRecursively(projectDir)

  def clean(): String = run("clean")

  def run(taskName: String): String = getRunner(taskName).build.getOutput

  def fail(taskName: String): String = getRunner(taskName).buildAndFail.getOutput

  private def getRunner(taskName: String): GradleRunner = GradleRunner.create
    .withProjectDir(projectDir)
    .withArguments("-i", taskName)
}
