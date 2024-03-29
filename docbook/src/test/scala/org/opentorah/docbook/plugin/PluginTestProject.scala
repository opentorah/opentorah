package org.opentorah.docbook.plugin

import org.opentorah.docbook.{DocBook, Layout}
import org.opentorah.util.Files
import java.io.File
import org.gradle.testkit.runner.GradleRunner

class PluginTestProject private(
  projectDir: File,
  pluginRootDir: File,
  document: String,
  substitutions: Map[String, String],
  isPdfEnabled: Boolean,
  isMathJaxEnabled: Boolean,
  useMathJaxV3: Boolean
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
          |    useMathJaxV3 = $useMathJaxV3
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
    val result: GradleRunner = GradleRunner.create.withProjectDir(projectDir)
    if logInfo then result.withArguments(/*"-d",*/ "processDocBook") else result.withArguments("processDocBook")

object PluginTestProject:

  private val documentName: String = "test"

  def apply(
    prefix: String,
    name: String,
    document: String,
    substitutions: Map[String, String] = Map.empty,
    isPdfEnabled: Boolean = false,
    isMathJaxEnabled: Boolean = false,
    useMathJaxV3: Boolean = false
  ): PluginTestProject =
    // .../opentorah.org/docbook/build/resources/test/org/opentorah/docbook/plugin/anchor.txt
    val root: File = Files.url2file(getClass.getResource("anchor.txt"))
      .getParentFile
      .getParentFile
      .getParentFile
      .getParentFile
      .getParentFile
      .getParentFile
      .getParentFile
      .getParentFile

    val result: PluginTestProject = new PluginTestProject(
      projectDir = Files.file(root, "build", prefix, name),
      pluginRootDir = root.getParentFile,
      document,
      substitutions,
      isPdfEnabled,
      isMathJaxEnabled,
      useMathJaxV3
    )

    result.write()

    result
