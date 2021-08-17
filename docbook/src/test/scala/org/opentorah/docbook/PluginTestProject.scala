package org.opentorah.docbook

import java.io.File
import org.gradle.testkit.runner.GradleRunner
import org.opentorah.util.Files

class PluginTestProject private(
  projectDir: File,
  pluginRootDir: File,
  document: String,
  substitutions: Map[String, String],
  isPdfEnabled: Boolean,
  isMathJaxEnabled: Boolean,
  useJ2V8: Boolean
) {
  projectDir.mkdirs()

  val layout: Layout = Layout.forRoot(projectDir)

  def write(): Unit = {
    val documentName: String = "test"

    // reference plugin's root project
    Files.write(
      file = new File(projectDir, "settings.gradle"),
      replace = true,
      content =
        s"""|includeBuild '$pluginRootDir'
            |""".stripMargin
    )

    val substitutionsFormatted: String = if (substitutions.isEmpty) "" else {
      val contents: String = substitutions.map { case (name: String, value: String) =>
        s""""$name": $value"""
      }.mkString(",\n")

      s"""
         |  substitutions = [
         |    $contents
         |  ]
         |""".stripMargin
    }

    val outputFormats: String = if (isPdfEnabled) """ "html", "pdf" """ else """ "html" """

    Files.write(
      file = new File(projectDir, "build.gradle"),
      replace = true,
      content =
      s"""|plugins {
          |  id 'org.opentorah.docbook' version '1.0.0'
          |  id 'base'
          |}
          |
          |repositories {
          |  mavenCentral()
          |}
          |
          |docBook {
          |  document = "$documentName"
          |  outputFormats = [$outputFormats]
          |$substitutionsFormatted
          |
          |  mathJax {
          |    enabled = $isMathJaxEnabled
          |    useJ2V8 = $useJ2V8
          |  }
          |}
          |""".stripMargin
    )

    Files.write(
      file = layout.forDocument(prefixed = false, documentName).inputFile,
      replace = false,
      content = document
    )
  }

  def destroy(): Unit = Files.deleteFiles(projectDir)

  def run(logInfo: Boolean = false): String = getRunner(logInfo).build.getOutput

  def fail(): String = getRunner(logInfo = false).buildAndFail.getOutput

  def indexHtml: String = saxonOutputFile(section.Html)

  def fo: String = saxonOutputFile(section.Pdf)

  private def saxonOutputFile(docBook2: org.opentorah.docbook.section.DocBook2): String =
    Files.read(
      layout.forDocument(prefixed = false, PluginTestProject.documentName)
        .saxonOutputFile(docBook2.defaultVariant)
    ).mkString("\n")

  private def getRunner(logInfo: Boolean): GradleRunner = {
    val result = GradleRunner.create.withProjectDir(projectDir)
    if (logInfo) result.withArguments("-d", "processDocBook") else result.withArguments("processDocBook")
  }
}

object PluginTestProject {

  private val documentName: String = "test"

  def apply(
    prefix: String,
    name: String,
    document: String,
    substitutions: Map[String, String] = Map.empty,
    isPdfEnabled: Boolean = false,
    isMathJaxEnabled: Boolean = false,
    useJ2V8: Boolean = false
  ): PluginTestProject = {
    val layout: Layout = Layout.forCurrent

    val result: PluginTestProject = new PluginTestProject(
      projectDir = Files.file(layout.buildDir, Seq(prefix, name)),
      pluginRootDir = layout.projectDir.getParentFile,
      document,
      substitutions,
      isPdfEnabled,
      isMathJaxEnabled,
      useJ2V8
    )

    result.write()

    result
  }
}
