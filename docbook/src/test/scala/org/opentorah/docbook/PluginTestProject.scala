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
):
  projectDir.mkdirs()

  def destroy(): Unit = Files.deleteFiles(projectDir)

  val layout: Layout = Layout.forRoot(projectDir)

  def indexHtml: String = Files.read(
    Files.file(layout.out, PluginTestProject.documentName, "html", "index.html")
  ).mkString("\n")

  def write(): Unit =
    val documentName: String = "test"

    // reference plugin's root project
    Files.write(
      file = File(projectDir, "settings.gradle"),
      replace = true,
      content =
        s"""|includeBuild '$pluginRootDir'
            |""".stripMargin
    )

    val substitutionsFormatted: String = if substitutions.isEmpty then "" else
      val contents: String = substitutions.map((name: String, value: String) =>
        s""""$name": $value"""
      ).mkString(",\n")

      s"""
         |      substitutions = [
         |        $contents
         |      ]
         |""".stripMargin

    val outputFormats: String = if isPdfEnabled then "'html', 'pdf'" else "'html'"

    Files.write(
      file = File(projectDir, "build.gradle"),
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
          |  math {
          |    mathJaxEnabled = $isMathJaxEnabled
          |    useJ2V8 = $useJ2V8
          |  }
          |
          |  documents {
          |    $documentName {
          |      output = [$outputFormats]
          |      $substitutionsFormatted
          |    }
          |  }
          |}
          |""".stripMargin
    )

    Files.write(
      file = File(layout.input, PluginTestProject.documentName + ".xml"),
      replace = false,
      content = document
    )

  def run(logInfo: Boolean = false): String = getRunner(logInfo).build.getOutput

  def fail(): String = getRunner(logInfo = false).buildAndFail.getOutput

  private def getRunner(logInfo: Boolean): GradleRunner =
    val result = GradleRunner.create.withProjectDir(projectDir)
    if logInfo then result.withArguments("-d", "processDocBook") else result.withArguments("processDocBook")

object PluginTestProject:

  private val documentName: String = "test"

  def apply(
    prefix: String,
    name: String,
    document: String,
    substitutions: Map[String, String] = Map.empty,
    isPdfEnabled: Boolean = false,
    isMathJaxEnabled: Boolean = false,
    useJ2V8: Boolean = false
  ): PluginTestProject =
    val root: File = File(".").getAbsoluteFile.getParentFile
    val result: PluginTestProject = new PluginTestProject(
      projectDir = Files.file(root, "build", prefix, name),
      pluginRootDir = root.getParentFile,
      document,
      substitutions,
      isPdfEnabled,
      isMathJaxEnabled,
      useJ2V8
    )

    result.write()

    result
