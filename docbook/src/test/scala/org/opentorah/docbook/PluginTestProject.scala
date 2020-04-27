package org.opentorah.docbook

import java.io.File
import org.gradle.testkit.runner.GradleRunner
import org.opentorah.docbook.plugin.{DocBook, Layout}
import org.opentorah.fop.util.{Logger, TestLogger}
import org.opentorah.fop.xml.Xml
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
    val logger: Logger = new TestLogger

    def writeInto(file: File, replace: Boolean)(content: String): Unit =
      org.opentorah.fop.util.Files.writeInto(file, replace, content, logger)

    val documentName: String = "test"

    // reference plugin's root project
    writeInto(layout.settingsGradle, replace = true)(content =
      s"""|includeBuild '$pluginRootDir'
          |""")

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

    val outputFormats: String = if (isPdfEnabled) """ "html", "pdf" """ else """ "html" """

    writeInto(layout.buildGradle, replace = true)(content =
      s"""|plugins {
          |  id 'org.podval.docbook-gradle-plugin' version '1.0.0'
          |  id 'base'
          |}
          |
          |repositories {
          |  jcenter()
          |}
          |
          |docBook {
          |  document = "$documentName"
          |  outputFormats = [$outputFormats]
          |$substitutionsFormatted
          |
          |  mathJax {
          |    isEnabled = $isMathJaxEnabled
          |    useJ2V8 = $useJ2V8
          |  }
          |}
          |"""
    )

    writeInto(layout.inputFile(documentName), replace = false)(s"${Xml.header}\n$document")
  }

  def destroy(): Unit = Files.deleteFiles(projectDir)

  def run(logInfo: Boolean = false): String = getRunner(logInfo).build.getOutput

  def fail(): String = getRunner(logInfo = false).buildAndFail.getOutput

  def indexHtml: String = saxonOutputFile(section.Html)

  def fo: String = saxonOutputFile(section.Pdf)

  private def saxonOutputFile(docBook2: org.opentorah.docbook.section.DocBook2): String =
    Files.read1(layout.forDocument(prefixed = false, PluginTestProject.documentName).saxonOutputFile(docBook2))

  private def getRunner(logInfo: Boolean): GradleRunner = {
    val result = GradleRunner.create.withProjectDir(projectDir)
    if (logInfo) result.withArguments("-d", "processDocBook") else result.withArguments("processDocBook")
  }
}

object PluginTestProject {

  private val documentName: String = "test"

  def apply(
    name: String,
    prefix: Option[String] = None,
    document: String = s"<article ${DocBook.Namespace.withVersion}/>",
    substitutions: Map[String, String] = Map.empty,
    isPdfEnabled: Boolean = false,
    isMathJaxEnabled: Boolean = false,
    useJ2V8: Boolean = false
  ): PluginTestProject = {
    val layout: Layout = Layout.forCurrent
    val result: PluginTestProject = new PluginTestProject(
      projectDir = new File(Files.prefixedDirectory(layout.buildDir, prefix), name),
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
